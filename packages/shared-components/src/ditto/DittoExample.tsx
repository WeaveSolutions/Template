/**
 * Ditto Example Component
 * 
 * This component demonstrates how to use the Ditto provider
 * and interact with Ditto collections in a cross-platform way.
 */

import React, { useEffect, useState } from 'react';
import { View, Text, Button, StyleSheet, Platform, TextInput } from 'react-native';
import { useDittoContext } from '../../../shared-provider/src/ditto/DittoProvider';

// Define the data structure for a Todo item
interface Todo {
  id: string;
  text: string;
  completed: boolean;
  createdAt: number;
}

export function DittoExample() {
  const { ditto, isInitialized, isOnline, isSyncing, syncNow, error } = useDittoContext();
  const [todos, setTodos] = useState<Todo[]>([]);
  const [newTodoText, setNewTodoText] = useState('');
  const [subscription, setSubscription] = useState<any>(null);
  
  // Initialize and subscribe to the todos collection when Ditto is ready
  useEffect(() => {
    if (!ditto || !isInitialized) {
      return;
    }
    
    // Create a subscription to the todos collection
    try {
      const collection = ditto.collection('todos');
      const sub = collection.find().subscribe();
      
      // Set up the live query to update the todos whenever the collection changes
      const liveQuery = collection.find().observeLocal((docs) => {
        const newTodos = docs.map(doc => {
          const data = doc.value;
          return {
            id: doc.id,
            text: data.text || '',
            completed: data.completed || false,
            createdAt: data.createdAt || Date.now()
          };
        });
        
        setTodos(newTodos);
      });
      
      setSubscription({ sub, liveQuery });
      
      // Clean up function
      return () => {
        if (liveQuery) {
          liveQuery.cancel();
        }
        if (sub) {
          sub.cancel();
        }
      };
    } catch (err) {
      console.error('Error setting up Ditto subscription:', err);
    }
  }, [ditto, isInitialized]);
  
  // Function to add a new todo
  const addTodo = async () => {
    if (!ditto || !isInitialized || !newTodoText) {
      return;
    }
    
    try {
      await ditto.collection('todos').upsert({
        text: newTodoText,
        completed: false,
        createdAt: Date.now()
      });
      setNewTodoText('');
    } catch (err) {
      console.error('Error adding todo:', err);
    }
  };
  
  // Function to toggle a todo's completed status
  const toggleTodo = async (id: string, completed: boolean) => {
    if (!ditto || !isInitialized) {
      return;
    }
    
    try {
      await ditto.collection('todos')
        .findById(id)
        .update(doc => {
          doc.set('completed', !completed);
        });
    } catch (err) {
      console.error('Error toggling todo:', err);
    }
  };
  
  // Function to remove a todo
  const removeTodo = async (id: string) => {
    if (!ditto || !isInitialized) {
      return;
    }
    
    try {
      await ditto.collection('todos')
        .findById(id)
        .remove();
    } catch (err) {
      console.error('Error removing todo:', err);
    }
  };
  
  // Render loading state if Ditto is not initialized
  if (!isInitialized) {
    return (
      <View style={styles.container}>
        <Text>Loading Ditto...</Text>
      </View>
    );
  }
  
  // Render error state if there's an error
  if (error) {
    return (
      <View style={styles.container}>
        <Text style={styles.errorText}>Error: {error.message}</Text>
      </View>
    );
  }
  
  return (
    <View style={styles.container}>
      <Text style={styles.heading}>Ditto Todos Example</Text>
      
      <View style={styles.statusContainer}>
        <Text>Status: {isInitialized ? 'Connected' : 'Disconnected'}</Text>
        <Text>Online: {isOnline ? 'Yes' : 'No'}</Text>
        <Text>Syncing: {isSyncing ? 'Yes' : 'No'}</Text>
      </View>
      
      <View style={styles.inputContainer}>
        {Platform.OS === 'web' ? (
          <input 
            type="text"
            value={newTodoText}
            onChange={(e) => setNewTodoText(e.target.value)}
            placeholder="Add a new todo"
            style={styles.input as any}
          />
        ) : (
          <TextInput
            value={newTodoText}
            onChangeText={setNewTodoText}
            placeholder="Add a new todo"
            style={styles.input}
          />
        )}
        <Button title="Add" onPress={addTodo} />
      </View>
      
      <View style={styles.todoList}>
        {todos.length === 0 ? (
          <Text>No todos yet. Add your first one!</Text>
        ) : (
          todos.map(todo => (
            <View key={todo.id} style={styles.todoItem}>
              <Text 
                style={[
                  styles.todoText, 
                  todo.completed && styles.completedTodo
                ]}
                onPress={() => toggleTodo(todo.id, todo.completed)}
              >
                {todo.text}
              </Text>
              <Button 
                title="Remove" 
                onPress={() => removeTodo(todo.id)}
                color="red"
              />
            </View>
          ))
        )}
      </View>
      
      <View style={styles.syncContainer}>
        <Button 
          title="Sync Now" 
          onPress={syncNow}
          disabled={isSyncing}
        />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    padding: 16,
    flex: 1,
  },
  heading: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 16,
  },
  statusContainer: {
    marginBottom: 16,
    padding: 8,
    backgroundColor: '#f0f0f0',
    borderRadius: 4,
  },
  inputContainer: {
    flexDirection: 'row',
    marginBottom: 16,
    alignItems: 'center',
  },
  input: {
    flex: 1,
    marginRight: 8,
    padding: 8,
    borderWidth: 1,
    borderColor: '#ccc',
    borderRadius: 4,
  },
  todoList: {
    flex: 1,
  },
  todoItem: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between',
    padding: 8,
    marginBottom: 8,
    backgroundColor: '#f9f9f9',
    borderRadius: 4,
  },
  todoText: {
    flex: 1,
    marginRight: 8,
  },
  completedTodo: {
    textDecorationLine: 'line-through',
    color: '#888',
  },
  syncContainer: {
    marginTop: 16,
  },
  errorText: {
    color: 'red',
    fontWeight: 'bold',
  },
});

export default DittoExample;
