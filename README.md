# Logic Circuit Simulator
This project enhances a basic logical circuit simulator by introducing time control and gate delays, making it suitable for simulating synchronous circuits. Below are the key functionalities and components of the project.
1. **Time simulation**: Includes a mutable current time and an event queue to schedule and process events at specific times.
2. **Gate delays**: Outputs from logic gates are updated after a defined delay, allowing realistic simulations.

Supported gates: NOT, AND, NAND, OR, NOR, XOR. Includes a pre-implemented flip-flop circuit as an example of building stateful systems with logic gates.

# How It Works
- Actions associated with wires or events are executed in chronological order.
- Logic gates use the state of input wires at the time of processing to determine their output, ensuring consistent behavior.
- The simulator's modular design allows building complex circuits, including sequential logic components like flip-flops.

# Key Features
**Wires**: Connect components in the circuit. Each wire holds a logical value and triggers actions upon state changes. \
**Simulation structure**: Encapsulates simulation time and an event queue, enabling multiple independent simulations. \
**Event queue**: A priority-based structure using heaps to manage and execute scheduled events chronologically. \
**Time control**: The simulation progresses in time using the sim-wait! function, executing all events whose time has arrived.
