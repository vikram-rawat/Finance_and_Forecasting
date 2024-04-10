import torch
import torch.nn as nn
import numpy as np
import matplotlib.pyplot as plt

# Generate random data
X = np.arange(0.001, 0.101, 0.001).reshape(-1, 1)
y = 2 * X + 1

len(X)
# Convert data to tensors
X_tensor = torch.from_numpy(X).float()
y_tensor = torch.from_numpy(y).float()

# Define model


class LinearRegression(nn.Module):
    def __init__(self):
        super(LinearRegression, self).__init__()
        self.linear = nn.Linear(1, 1)

    def forward(self, x):
        out = self.linear(x)
        return out


model = LinearRegression()

# Define loss function and optimizer
criterion = nn.MSELoss()
optimizer = torch.optim.SGD(model.parameters(), lr=0.1)

# Train model
num_epochs = 12000
for epoch in range(num_epochs):
    # Forward pass
    y_pred = model(X_tensor)

    # Compute loss
    loss = criterion(y_pred, y_tensor)

    # Backward pass and optimize weights
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    # Print progress
    if (epoch+1) % 100 == 0:
        print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {loss.item():.4f}')

# Plot results
predicted = model(X_tensor).detach().numpy()
plt.plot(X, y, 'ro', label='Original data')
plt.plot(X, predicted, label='Fitted line')
plt.legend()
plt.show()
