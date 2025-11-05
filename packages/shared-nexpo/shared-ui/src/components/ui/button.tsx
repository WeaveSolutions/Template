import React from 'react';
import { Pressable, Text, Platform } from 'react-native';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../../lib/utils';

const buttonVariants = cva(
  "flex items-center justify-center rounded-md",
  {
    variants: {
      variant: {
        default: "bg-primary-500 text-white",
        destructive: "bg-red-500 text-white",
        outline: "border border-primary-500 text-primary-500",
        secondary: "bg-gray-200 text-gray-800",
        ghost: "hover:bg-gray-100 text-gray-800",
        link: "text-primary-500 underline-offset-4 hover:underline",
      },
      size: {
        default: "h-10 px-4 py-2",
        sm: "h-8 px-3 py-1",
        lg: "h-12 px-6 py-3",
        icon: "h-10 w-10",
      },
    },
    defaultVariants: {
      variant: "default",
      size: "default",
    },
  }
);

export interface ButtonProps extends VariantProps<typeof buttonVariants> {
  children: any; // Using 'any' instead of ReactNode for cross-platform compatibility
  className?: string;
  textClassName?: string;
  onPress?: () => void;
  disabled?: boolean;
}

export function Button({
  children,
  className,
  textClassName,
  variant,
  size,
  onPress,
  disabled,
  ...props
}: ButtonProps) {
  // For web, we can use className directly
  // For native, we need to use style prop with tailwind-rn
  const buttonClasses = cn(
    buttonVariants({ variant, size }),
    disabled && "opacity-50",
    className
  );

  // For text styling
  const textClasses = cn(
    "font-medium",
    variant === "default" && "text-white",
    variant === "destructive" && "text-white",
    variant === "outline" && "text-primary-500",
    variant === "secondary" && "text-gray-800",
    variant === "ghost" && "text-gray-800",
    variant === "link" && "text-primary-500",
    disabled && "opacity-50",
    textClassName
  );

  return (
    <Pressable
      className={buttonClasses}
      onPress={onPress}
      disabled={disabled}
      {...props}
    >
      {typeof children === 'string' ? (
        <Text className={textClasses}>{children}</Text>
      ) : (
        children
      )}
    </Pressable>
  );
}
