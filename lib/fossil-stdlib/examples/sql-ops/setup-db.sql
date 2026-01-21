-- Setup script for SQLite example database
-- Run with: sqlite3 shop.db < setup-db.sql

CREATE TABLE IF NOT EXISTS customers (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    city TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS products (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    price REAL NOT NULL,
    category TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS orders (
    id INTEGER PRIMARY KEY,
    customer_id INTEGER NOT NULL,
    product_id INTEGER NOT NULL,
    quantity INTEGER NOT NULL,
    order_date TEXT NOT NULL,
    FOREIGN KEY (customer_id) REFERENCES customers(id),
    FOREIGN KEY (product_id) REFERENCES products(id)
);

-- Insert sample customers
INSERT INTO customers (id, name, email, city) VALUES
(1, 'Alice Smith', 'alice@example.com', 'New York'),
(2, 'Bob Johnson', 'bob@example.com', 'Los Angeles'),
(3, 'Carol Williams', 'carol@example.com', 'Chicago'),
(4, 'David Brown', 'david@example.com', 'Houston'),
(5, 'Eve Davis', 'eve@example.com', 'Phoenix');

-- Insert sample products
INSERT INTO products (id, name, price, category) VALUES
(1, 'Laptop', 999.99, 'Electronics'),
(2, 'Smartphone', 599.99, 'Electronics'),
(3, 'Headphones', 149.99, 'Electronics'),
(4, 'Desk Chair', 299.99, 'Furniture'),
(5, 'Standing Desk', 449.99, 'Furniture'),
(6, 'Coffee Maker', 79.99, 'Appliances'),
(7, 'Blender', 49.99, 'Appliances');

-- Insert sample orders
INSERT INTO orders (id, customer_id, product_id, quantity, order_date) VALUES
(1, 1, 1, 1, '2024-01-15'),
(2, 1, 3, 2, '2024-01-16'),
(3, 2, 2, 1, '2024-01-17'),
(4, 3, 4, 1, '2024-01-18'),
(5, 3, 5, 1, '2024-01-18'),
(6, 4, 6, 2, '2024-01-19'),
(7, 5, 1, 1, '2024-01-20'),
(8, 5, 7, 1, '2024-01-20');
