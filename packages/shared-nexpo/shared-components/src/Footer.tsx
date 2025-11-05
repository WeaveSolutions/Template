import React, { useState, useEffect } from 'react';
import { View, Text, Pressable, Platform } from 'react-native';
import { Link } from 'solito/link';
import { useTheme } from './ThemeContext';

export function Footer() {
  const { theme } = useTheme();

  const currentYear = new Date().getFullYear();

  const footerLinks = [
    { label: 'Home', href: '/' },
    { label: 'About', href: '/about' },
    { label: 'Contact', href: '/contact' },
    { label: 'Privacy', href: '/privacy' },
  ];

  const socialLinks = [
    { label: 'GitHub', href: 'https://github.com' },
    { label: 'Twitter', href: 'https://twitter.com' },
    { label: 'LinkedIn', href: 'https://linkedin.com' },
  ];

  return (
    <View
      style={{
        backgroundColor: theme.colors.surface,
        borderTopWidth: 1,
        borderTopColor: theme.colors.border,
        paddingVertical: 32,
        paddingHorizontal: 16,
        marginTop: 'auto',
      }}
    >
      <View
        style={{
          maxWidth: 1200,
          width: '100%',
          marginHorizontal: 'auto',
        }}
      >
        {/* Links Section */}
        <View
          style={{
            flexDirection: Platform.OS === 'web' ? 'row' : 'column',
            flexWrap: 'wrap',
            justifyContent: 'center',
            marginBottom: 24,
            gap: Platform.OS === 'web' ? 24 : 12,
          }}
        >
          {footerLinks.map((link) => (
            <Link key={link.href} href={link.href}>
              <Pressable
                style={({ hovered }: any) => ({
                  opacity: hovered ? 0.7 : 1,
                })}
              >
                <Text
                  style={{
                    color: theme.colors.textSecondary,
                    fontSize: 14,
                    textAlign: 'center',
                  }}
                >
                  {link.label}
                </Text>
              </Pressable>
            </Link>
          ))}
        </View>

        {/* Social Links */}
        <View
          style={{
            flexDirection: 'row',
            justifyContent: 'center',
            gap: 20,
            marginBottom: 24,
          }}
        >
          {socialLinks.map((link) => (
            <Pressable
              key={link.href}
              onPress={() => {
                if (Platform.OS === 'web') {
                  window.open(link.href, '_blank');
                }
              }}
              style={({ hovered }: any) => ({
                opacity: hovered ? 0.7 : 1,
              })}
            >
              <Text
                style={{
                  color: theme.colors.textSecondary,
                  fontSize: 14,
                }}
              >
                {link.label}
              </Text>
            </Pressable>
          ))}
        </View>

        {/* Copyright */}
        <Text
          style={{
            color: theme.colors.textSecondary,
            fontSize: 12,
            textAlign: 'center',
          }}
        >
          {currentYear} Your Company. All rights reserved.
        </Text>

        {/* Built with section */}
        <View
          style={{
            marginTop: 16,
            alignItems: 'center',
          }}
        >
          <Text
            style={{
              color: theme.colors.textSecondary,
              fontSize: 12,
              textAlign: 'center',
            }}
          >
            Built with <Text style={{ color: theme.colors.primary }}>❤️</Text> using Next.js, Expo & Solito
          </Text>
        </View>
      </View>
    </View>
  );
}
