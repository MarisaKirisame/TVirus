import pydot
import subprocess

class UF:
    def __init__(self, v):
        self.v = v
        self.parent = None

    def get_root(self):
        if self.parent is None:
            return self
        else:
            self.parent = self.parent.get_root()
            return self.parent

    def value(self):
        return self.get_root().v

    def merge(self, rhs_):
        lhs = self.get_root()
        rhs = rhs_.get_root()
        if lhs != rhs:
            assert lhs.parent is None
            assert rhs.parent is None
            lhs.parent = rhs
            rhs.v += lhs.v

class Stat:
    def __init__(self, cost, alive):
        self.cost = cost
        self.alive = alive

    def __add__(self, rhs):
        return Stat(self.cost + rhs.cost, self.alive + rhs.alive)

    def __repr__(self):
        return repr((self.cost, self.alive))

class Node:
    def __init__(self, name):
        self.name = name
        self.cost = UF(Stat(1, 1))

class Graph:
    def __init__(self):
        self.nodes = set()
        self.alive = set()
        self.edges = {}
        self.backedges = {}
        self.max_cost = 0
        self.last_evicted = None

    def node(self):
        n = Node(str(len(self.nodes)))
        self.nodes.add(n)
        self.alive.add(n)
        self.edges[n] = set()
        self.backedges[n] = set()
        return n

    def edge(self, l, r):
        self.edges[l].add(r)
        self.backedges[r].add(l)

    def cost(self, n):
        counted = set()
        counted.add(n.cost.get_root())
        value = n.cost.value()
        for neighbor in self.edges[n]:
            if neighbor.cost.get_root() not in counted:
                counted.add(neighbor.cost.get_root())
                value += neighbor.cost.value()
        for neighbor in self.backedges[n]:
            if neighbor.cost.get_root() not in counted:
                counted.add(neighbor.cost.get_root())
                value += neighbor.cost.value()
        return value.cost / (value.alive ** 0.5)

    def evict_node(self, n):
        assert isinstance(n, Node)
        for neighbor in self.edges[n]:
            n.cost.merge(neighbor.cost)
        for neighbor in self.backedges[n]:
            n.cost.merge(neighbor.cost)
        cost = n.cost.value().cost / (n.cost.value().alive ** 0.5)
        n.cost.value().alive -= 1
        self.max_cost = max(self.max_cost, cost)
        self.alive.remove(n)
        self.last_evicted = n
        return cost

    def evict(self):
        assert len(self.alive) > 0
        n = None
        min_cost = None
        for candidate in self.alive:
            assert candidate is not None
            cost = self.cost(candidate)
            if min_cost is None or cost < min_cost:
                n = candidate
                min_cost = cost
        evicted_cost = self.evict_node(n)
        assert min_cost == evicted_cost

    def evict_to(self, limit):
        while len(self.alive) > limit:
            print(len(self.alive), self.max_cost)
            self.evict()

g = Graph()

size = 30

pascal = False

if pascal:
    prev_l = None
    l = [g.node() for _ in range(size)]

    for _ in range(1, size):
        next_l = [g.node() for _ in range(size)]
        for i in range(1, size - 1):
            g.edge(l[i], next_l[i])
            g.edge(l[i-1], next_l[i])
            g.edge(l[i+1], next_l[i])
        # turning it on make very ugly svg and we already can handle this case.
        if False and prev_l is not None:
            for i in range(1, size - 1):
                g.edge(prev_l[i], next_l[i])
                g.edge(prev_l[i-1], next_l[i])
                g.edge(prev_l[i+1], next_l[i])
        prev_l = l
        l = next_l
else:
    l = [g.node() for _ in range(size)]
    for i in range(1, size):
        g.edge(l[i-1], l[i])
    for _ in range(1, size):
        next_l = [g.node() for _ in range(size)]
        for i in range(1, size):
            g.edge(next_l[i-1], next_l[i])
        for i in range(size):
            g.edge(l[i], next_l[i])
        l = next_l

g.evict_to(300)
print(g.max_cost)

edge_to_name = {}
graph = pydot.Dot("my_graph", graph_type="graph")

for n in g.nodes:
    if n in g.alive:
        color = "red"
    elif n == g.last_evicted:
        color = "gold"
    else:
        color = "black"
    if n.cost is None:
        name = n.name
    else:
        name = f"{n.name}({n.cost.value()})"
    edge_to_name[n] = name
    graph.add_node(pydot.Node(name, color=color))

for l in g.edges:
    for r in g.edges[l]:
        graph.add_edge(pydot.Edge(edge_to_name[l], edge_to_name[r]))

graph.write_svg(f"output.svg")
subprocess.run("xdg-open output.svg", shell=True)

