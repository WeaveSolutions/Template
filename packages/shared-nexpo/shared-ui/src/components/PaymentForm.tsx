import React, { useState } from 'react';
import { View, Text, TextInput, Button, StyleSheet, Platform } from 'react-native';
import { CardElement, useStripe, useElements } from '@stripe/react-stripe-js';
import { useStripeContext } from '../context/StripeContext';

const PaymentForm = ({ onPaymentSuccess }: { onPaymentSuccess: () => void }) => {
  const { stripe } = useStripeContext();
  const elements = useElements();
  const [name, setName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [processing, setProcessing] = useState(false);

  const handleSubmit = async (event: any) => {
    event.preventDefault();
    if (!stripe || !elements) {
      return;
    }

    setProcessing(true);
    const cardElement = elements.getElement(CardElement);

    if (!cardElement) {
      setError('Card element not found');
      setProcessing(false);
      return;
    }

    const { error, paymentMethod } = await stripe.createPaymentMethod({
      type: 'card',
      card: cardElement,
      billing_details: {
        name,
      },
    });

    if (error) {
      setError(error.message || 'An error occurred');
      setProcessing(false);
    } else {
      // Here you would typically send the paymentMethod.id to your server for further processing
      // For this example, we'll just simulate a successful payment
      setTimeout(() => {
        setProcessing(false);
        onPaymentSuccess();
      }, 1000);
    }
  };

  return (
    <View style={styles.container}>
      <Text style={styles.title}>Payment Information</Text>
      <TextInput
        style={styles.input}
        placeholder="Name on card"
        value={name}
        onChangeText={setName}
      />
      {Platform.OS === 'web' ? (
        <form onSubmit={handleSubmit} style={styles.form}>
          <CardElement
            options={{
              style: {
                base: {
                  fontSize: '16px',
                  color: '#424770',
                  '::placeholder': {
                    color: '#aab7c4',
                  },
                },
                invalid: {
                  color: '#9e2146',
                },
              },
            }}
          />
          {error && <Text style={styles.error}>{error}</Text>}
          <Button
            title={processing ? 'Processing...' : 'Pay Now'}
            onPress={() => {}}
            disabled={processing || !stripe}
          />
        </form>
      ) : (
        <Text style={styles.mobileNote}>Mobile payment processing will be implemented separately</Text>
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    padding: 20,
    width: '100%',
    maxWidth: 400,
  },
  title: {
    fontSize: 18,
    fontWeight: 'bold',
    marginBottom: 15,
  },
  input: {
    height: 40,
    borderColor: 'gray',
    borderWidth: 1,
    marginBottom: 15,
    paddingHorizontal: 10,
  },
  form: {
    width: '100%',
  },
  error: {
    color: 'red',
    marginTop: 10,
  },
  mobileNote: {
    color: 'gray',
    marginTop: 10,
    fontStyle: 'italic',
  },
});

export default PaymentForm;
