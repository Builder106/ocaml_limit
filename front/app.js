/**
 * Bloomberg Terminal Edition - High-Performance Matching Engine
 */

class LOBTerminal {
    constructor() {
        this.ws = null;
        this.reconnectInterval = 3000;
        this.depthChart = null;
        this.maxTapeEntries = 50;
        this.maxLogEntries = 30;
        
        // State
        this.bids = [];
        this.asks = [];
        this.trades = [];
        
        this.init();
    }

    init() {
        this.initCharts();
        this.connect();
        this.startClock();
        this.startMockData(); // Demo mode if no WS
        this.bindEvents();
    }

    bindEvents() {
        window.placeOrder = (side) => this.handleOrderSubmit(side);
    }

    startClock() {
        const clockEl = document.getElementById('terminal-clock');
        setInterval(() => {
            const now = new Date();
            clockEl.innerText = now.getFullYear() + '-' + 
                              String(now.getMonth() + 1).padStart(2, '0') + '-' + 
                              String(now.getDate()).padStart(2, '0') + ' ' + 
                              now.toTimeString().split(' ')[0];
        }, 1000);
    }

    connect() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${window.location.host}/ws`;
        
        this.ws = new WebSocket(wsUrl);

        this.ws.onopen = () => {
            console.log('Terminal Link Established');
            this.updateStatus(true);
        };

        this.ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.handleMessage(data);
        };

        this.ws.onclose = () => {
            this.updateStatus(false);
            setTimeout(() => this.connect(), this.reconnectInterval);
        };
    }

    updateStatus(connected) {
        const badge = document.getElementById('status-badge');
        if (connected) {
            badge.innerHTML = '<span class="w-2 h-2 bg-terminal-green rounded-full"></span> LIVE';
            badge.className = 'flex items-center gap-2 text-terminal-green';
        } else {
            badge.innerHTML = '<span class="w-2 h-2 bg-terminal-red rounded-full animate-pulse"></span> DISCONNECTED';
            badge.className = 'flex items-center gap-2 text-terminal-red';
        }
    }

    handleMessage(data) {
        switch (data.type) {
            case 'SNAPSHOT':
                this.asks = data.asks;
                this.bids = data.bids;
                this.renderBook();
                this.renderDepth();
                break;
            case 'TRADE':
                this.addTrade(data.trade);
                break;
            case 'STATS':
                this.updateStats(data.latency, data.throughput);
                break;
            case 'RISK_ALERT':
                this.addRiskLog('ALERT', data.message);
                break;
        }
    }

    renderBook() {
        const askContainer = document.getElementById('ask-book');
        const bidContainer = document.getElementById('bid-book');
        
        // Calculate max cumulative for normalization
        const allLevels = [...this.asks, ...this.bids];
        const maxCumulative = Math.max(...allLevels.map(l => l.totalSize)) || 1;

        // Render Asks (Sell) - Background bars on left
        askContainer.innerHTML = this.asks.map(a => {
            const width = (a.totalSize / maxCumulative) * 100;
            return `
                <div class="grid grid-cols-3 px-3 py-[2px] relative group cursor-pointer hover:bg-white/5 data-row">
                    <div class="absolute left-0 h-full heatmap-ask transition-all duration-300" style="width: ${width}%"></div>
                    <div class="text-terminal-red font-bold z-10">${a.price.toFixed(2)}</div>
                    <div class="text-right text-gray-300 z-10">${a.size.toLocaleString()}</div>
                    <div class="text-right text-gray-500 z-10">${a.totalSize.toLocaleString()}</div>
                </div>
            `;
        }).join('');

        // Render Bids (Buy) - Background bars on right
        bidContainer.innerHTML = this.bids.map(b => {
            const width = (b.totalSize / maxCumulative) * 100;
            return `
                <div class="grid grid-cols-3 px-3 py-[2px] relative group cursor-pointer hover:bg-white/5 data-row">
                    <div class="absolute right-0 h-full heatmap-bid transition-all duration-300" style="width: ${width}%"></div>
                    <div class="text-terminal-green font-bold z-10">${b.price.toFixed(2)}</div>
                    <div class="text-right text-gray-300 z-10">${b.size.toLocaleString()}</div>
                    <div class="text-right text-gray-500 z-10">${b.totalSize.toLocaleString()}</div>
                </div>
            `;
        }).join('');

        // Update Spread
        if (this.asks.length > 0 && this.bids.length > 0) {
            const bestAsk = this.asks[0].price;
            const bestBid = this.bids[0].price;
            const spread = (bestAsk - bestBid).toFixed(2);
            document.getElementById('spread').innerText = spread;
        }
    }

    addTrade(trade) {
        const tape = document.getElementById('trade-tape');
        const row = document.createElement('div');
        row.className = 'grid grid-cols-4 px-1 py-[1px] hover:bg-white/5 border-b border-terminal-border/50 text-[11px]';
        
        const timestamp = new Date().toTimeString().split(' ')[0] + '.' + String(Date.now() % 1000).padStart(3, '0');
        const colorClass = trade.side === 'BUY' ? 'text-terminal-green' : 'text-terminal-red';
        
        row.innerHTML = `
            <div class="text-gray-600">${timestamp}</div>
            <div class="text-right ${colorClass} font-bold">${trade.price.toFixed(2)}</div>
            <div class="text-right text-white">${trade.size}</div>
            <div class="text-right ${colorClass} font-black text-[9px] uppercase">${trade.side}</div>
        `;

        tape.prepend(row);
        if (tape.children.length > this.maxTapeEntries) {
            tape.removeChild(tape.lastChild);
        }
    }

    updateStats(latency, throughput) {
        document.getElementById('stat-latency').innerText = `${latency} μs`;
        document.getElementById('stat-throughput').innerText = `${throughput} orders/s`;
    }

    addRiskLog(type, message) {
        const log = document.getElementById('risk-log');
        const entry = document.createElement('div');
        const timestamp = new Date().toTimeString().split(' ')[0];
        
        let tagClass = 'text-terminal-green';
        if (type === 'WARNING' || type === 'ALERT') tagClass = 'text-terminal-amber';
        if (type === 'ERROR') tagClass = 'text-terminal-red';
        if (type === 'VERIFIED') tagClass = 'text-terminal-cyan';

        entry.innerHTML = `
            <span class="text-gray-600">${timestamp}</span>
            <span class="${tagClass} font-bold ml-2">[${type}]</span>
            <span class="text-gray-400 ml-2">${message}</span>
        `;
        
        log.prepend(entry);
        if (log.children.length > this.maxLogEntries) {
            log.removeChild(log.lastChild);
        }
    }

    initCharts() {
        const ctx = document.getElementById('depthChart').getContext('2d');
        this.depthChart = new Chart(ctx, {
            type: 'line',
            data: {
                datasets: [
                    {
                        label: 'Bids',
                        borderColor: '#00ff00',
                        backgroundColor: 'rgba(0, 255, 0, 0.2)',
                        fill: true,
                        stepped: true,
                        pointRadius: 0,
                        borderWidth: 1,
                        data: []
                    },
                    {
                        label: 'Asks',
                        borderColor: '#ff0000',
                        backgroundColor: 'rgba(255, 0, 0, 0.2)',
                        fill: true,
                        stepped: true,
                        pointRadius: 0,
                        borderWidth: 1,
                        data: []
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                animation: false,
                scales: {
                    x: {
                        type: 'linear',
                        grid: { color: '#111' },
                        ticks: { color: '#666', font: { size: 9, family: 'IBM Plex Mono' } }
                    },
                    y: {
                        position: 'right',
                        grid: { color: '#111' },
                        ticks: { color: '#666', font: { size: 9, family: 'IBM Plex Mono' } }
                    }
                },
                plugins: {
                    legend: { display: false }
                }
            }
        });
    }

    renderDepth() {
        if (!this.depthChart) return;
        
        const bidData = this.bids.map(b => ({ x: b.price, y: b.totalSize }));
        const askData = this.asks.map(a => ({ x: a.price, y: a.totalSize }));
        
        this.depthChart.data.datasets[0].data = bidData;
        this.depthChart.data.datasets[1].data = askData;
        this.depthChart.update('none');
    }

    handleOrderSubmit(side) {
        const price = parseFloat(document.getElementById('order-price').value);
        const size = parseInt(document.getElementById('order-qty').value);
        
        this.addRiskLog('OK', `Placing ${side} order: ${size} @ ${price}`);

        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                type: 'ORDER',
                side: side,
                price: price,
                size: size
            }));
        } else {
            this.addRiskLog('ALERT', `Gateway link down. Simulation mode active.`);
        }
    }

    // --- MOCK DATA FOR DEMO ---
    startMockData() {
        let basePrice = 18750.25;
        
        // Initial logs
        setTimeout(() => this.addRiskLog('OK', 'Network jitter detected within bounds'), 500);
        setTimeout(() => this.addRiskLog('VERIFIED', 'Risk limits within bounds'), 1000);
        setTimeout(() => this.addRiskLog('OK', 'Heartbeat received from gateway 1'), 1500);

        setInterval(() => {
            if (this.ws && this.ws.readyState === WebSocket.OPEN) return;

            basePrice += (Math.random() - 0.5) * 2;
            
            const generateLevels = (start, count, direction) => {
                let current = start;
                let total = 0;
                return Array.from({length: count}, (_, i) => {
                    const size = Math.floor(Math.random() * 5000) + 100;
                    total += size;
                    current += direction * 0.25;
                    return { price: current, size: size, totalSize: total };
                });
            };

            this.asks = generateLevels(basePrice + 0.25, 20, 1);
            this.bids = generateLevels(basePrice - 0.25, 20, -1);
            
            this.renderBook();
            this.renderDepth();
            this.updateStats((Math.random() * 2 + 5).toFixed(2), (Math.random() * 0.5 + 1.2).toFixed(2) + 'M');

            if (Math.random() > 0.4) {
                this.addTrade({
                    side: Math.random() > 0.5 ? 'BUY' : 'SELL',
                    price: basePrice + (Math.random() - 0.5),
                    size: Math.floor(Math.random() * 500)
                });
            }

            if (Math.random() > 0.95) {
                const logs = ['Volatility spike detected', 'Latency spiked to 12μs', 'Account balance verified', 'Position check symbol: MSFT'];
                const types = ['ALERT', 'WARNING', 'VERIFIED', 'OK'];
                const idx = Math.floor(Math.random() * logs.length);
                this.addRiskLog(types[idx], logs[idx]);
            }
        }, 1000);
    }
}

document.addEventListener('DOMContentLoaded', () => {
    new LOBTerminal();
});
