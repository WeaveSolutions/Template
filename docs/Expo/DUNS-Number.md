# DUNS Number Guide for Expo Apps

## What is a DUNS Number?

A DUNS (Data Universal Numbering System) number is a unique nine-digit identifier assigned to businesses by Dun & Bradstreet (D&B). It's required by Apple for organizations to enroll in the Apple Developer Program as a company or organization (not required for individual developers).

## Why Do You Need a DUNS Number?

### Required For:
- **Apple Developer Program enrollment** as an organization/company
- **App Store distribution** under a company name
- **Enterprise distribution** for internal apps
- **Volume Purchase Program** participation

### Not Required For:
- Individual developer accounts
- TestFlight beta testing (with individual account)
- Development and testing on personal devices

## How to Check if You Already Have a DUNS Number

1. Visit [D&B's DUNS lookup tool](https://www.dnb.com/duns-number/lookup.html)
2. Enter your company information
3. Search for existing DUNS numbers

## How to Obtain a DUNS Number

### Free Process (Standard - 30 days)
1. Go to [Apple's D&B page](https://developer.apple.com/enroll/duns-lookup/)
2. Click "Look up your D-U-N-S Number"
3. Search for your organization
4. If not found, request a new DUNS number
5. Fill out the required information:
   - Legal entity name
   - Headquarters address
   - Mailing address
   - Work phone number

### Expedited Process (Paid - 1-2 business days)
1. Contact D&B directly
2. Pay the expedite fee (typically $229 USD)
3. Receive DUNS number within 1-2 business days

## Information Needed for DUNS Registration

### Company Information
- **Legal entity name** (exactly as registered)
- **DBA (Doing Business As)** names if applicable
- **Physical address** (no P.O. boxes)
- **Mailing address** (if different)
- **Main phone number**
- **Number of employees**

### Contact Person
- **Full name**
- **Title/Position**
- **Direct phone number**
- **Email address**

### Business Details
- **Year established**
- **Business type** (Corporation, LLC, Partnership, etc.)
- **Primary SIC/NAICS code**
- **Line of business description**

## Common Issues and Solutions

### DUNS Number Not Recognized by Apple
- **Wait 24-48 hours** after receiving DUNS for Apple's system to update
- **Verify information matches** exactly between D&B and Apple
- **Contact Apple Developer Support** if issues persist

### Multiple DUNS Numbers
- Large organizations may have multiple DUNS numbers
- Use the headquarters or main entity DUNS
- Ensure consistency across all Apple accounts

### Information Mismatch
- Company name must match exactly
- Address must be current and match business registration
- Update D&B records before Apple enrollment

## Integration with Expo Workflow

### EAS Build and Submit
When using EAS (Expo Application Services) for building and submitting apps:

```json
// eas.json configuration
{
  "submit": {
    "production": {
      "ios": {
        "appleId": "your@email.com",
        "ascAppId": "1234567890",
        "appleTeamId": "XXXXXXXXXX"
      }
    }
  }
}
```

### Apple Team ID
Your Apple Team ID (different from DUNS) is required for:
- Code signing
- Push notifications
- App Store Connect API

Find your Team ID:
1. Log in to [Apple Developer account](https://developer.apple.com/account)
2. Go to Membership
3. Find Team ID under "Membership Information"

## Timeline Considerations

### Planning Your App Release
1. **DUNS Application**: 30 days (free) or 1-2 days (paid)
2. **Apple Developer Enrollment**: 24-48 hours after DUNS
3. **App Review**: 24-48 hours typically
4. **Total Timeline**: ~35 days minimum (free route)

### Recommended Timeline
- **6 weeks before launch**: Apply for DUNS (if needed)
- **4 weeks before launch**: Enroll in Apple Developer Program
- **2 weeks before launch**: Submit first build to App Store
- **1 week before launch**: Final testing and fixes

## Frequently Asked Questions

### Q: Can I develop without a DUNS number?
**A:** Yes, you can develop and test on simulators and personal devices. You only need it for App Store distribution as an organization.

### Q: Can I change from individual to organization later?
**A:** Yes, but it requires creating a new developer account and transferring apps, which can be complex.

### Q: Is DUNS required for Google Play?
**A:** No, Google Play Console doesn't require DUNS numbers.

### Q: What if my company is not in the US?
**A:** D&B operates globally. The process is similar but may vary by country.

### Q: Can I use a parent company's DUNS?
**A:** It depends on your corporate structure. Generally, use the entity that will own the apps.

## Useful Links

- [Apple Developer Program Enrollment](https://developer.apple.com/programs/enroll/)
- [D&B DUNS Lookup](https://www.dnb.com/duns-number/lookup.html)
- [Apple's DUNS Information](https://developer.apple.com/support/D-U-N-S/)
- [EAS Submit Documentation](https://docs.expo.dev/submit/introduction/)

## Checklist for App Store Submission

- [ ] DUNS number obtained
- [ ] Apple Developer account enrolled
- [ ] Team ID configured in EAS
- [ ] App Store Connect access granted
- [ ] Banking and tax information completed
- [ ] App privacy policy prepared
- [ ] App screenshots and metadata ready
- [ ] TestFlight beta testing completed

---

*Last updated: December 2024*