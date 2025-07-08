# AgriSupply Chain Verification Smart Contract

This Clarity smart contract enables tracking of agricultural products from farm to table, ensuring quality and fairness throughout the supply chain.

## Overview

The AgriSupply contract provides a transparent and immutable record of agricultural products as they move through the supply chain. It allows farmers to register their farms and products, and then record key events in the product lifecycle, from planting to harvest and beyond.

## Features

- Farm registration with location and certification details
- Product registration linked to specific farms
- Supply chain event tracking with timestamps, locations, and quality metrics
- Quality verification by authorized inspectors
- Complete product history accessible to all participants

## Contract Functions

### Farm Management

- `register-farm`: Register a new farm with name, location, and certification
- `deactivate-farm`: Mark a farm as inactive
- `get-farm`: Retrieve farm details

### Product Management

- `register-product`: Register a new agricultural product
- `record-harvest`: Record the harvest of a product
- `deactivate-product`: Mark a product as inactive
- `get-product`: Retrieve product details

### Supply Chain Events

- `add-supply-chain-event`: Record an event in the product's supply chain
- `get-supply-chain-event`: Retrieve details of a specific event
- `get-product-event-count`: Get the number of events for a product

### Inspector Management

- `add-inspector`: Add an authorized quality inspector
- `remove-inspector`: Remove an inspector's authorization
- `is-authorized-inspector`: Check if a principal is an authorized inspector

## Usage Example

1. Register a farm:
   ```
   (contract-call? .agrisupply register-farm "Green Acres Farm" "California, USA" "USDA Organic")
   ```

2. Register a product:
   ```
   (contract-call? .agrisupply register-product "Organic Tomatoes" u1 u100 "Vegetable" true)
   ```

3. Record harvest:
   ```
   (contract-call? .agrisupply record-harvest u1 u150)
   ```

4. Add supply chain event:
   ```
   (contract-call? .agrisupply add-supply-chain-event u1 "SHIPPING" "Distribution Center" i20 i60 u85 "Product in excellent condition")
   ```

5. Retrieve product history:
   ```
   (contract-call? .agrisupply get-product u1)
   (contract-call? .agrisupply get-supply-chain-event u1 u0)
   ```

## Security

The contract implements strict access controls:
- Only farm owners can register products for their farms
- Only product owners or authorized inspectors can add supply chain events
- Only the contract owner can manage inspector authorizations

## License

MIT