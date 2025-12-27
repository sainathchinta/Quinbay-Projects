package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;

public class AddressVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String addressId;
  private String streetAddress1;
  private String streetAddress2;
  private String kecamatan;
  private String kelurahan;
  private String city;
  private String state;
  private String postalCode;
  private String country;
  private String firstName;
  private String lastName;
  private String middleName;
  private String fullOrLegalName;
  private String phoneNumber;
  private String email;
  private String addressLabel;
  private boolean primary;

  public AddressVO() {

  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public String getAddressId() {
    return this.addressId;
  }

  public String getAddressLabel() {
    return this.addressLabel;
  }

  public String getCity() {
    return this.city;
  }

  public String getCountry() {
    return this.country;
  }

  public String getEmail() {
    return this.email;
  }

  public String getFirstName() {
    return this.firstName;
  }

  public String getFullOrLegalName() {
    return this.fullOrLegalName;
  }

  public String getKecamatan() {
    return this.kecamatan;
  }

  public String getKelurahan() {
    return this.kelurahan;
  }

  public String getLastName() {
    return this.lastName;
  }

  public String getMiddleName() {
    return this.middleName;
  }

  public String getPhoneNumber() {
    return this.phoneNumber;
  }

  public String getPostalCode() {
    return this.postalCode;
  }

  public String getState() {
    return this.state;
  }

  public String getStreetAddress1() {
    return this.streetAddress1;
  }

  public String getStreetAddress2() {
    return this.streetAddress2;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isPrimary() {
    return this.primary;
  }

  public void setAddressId(String addressId) {
    this.addressId = addressId;
  }

  public void setAddressLabel(String addressLabel) {
    this.addressLabel = addressLabel;
  }

  public void setCity(String city) {
    this.city = city;
  }

  public void setCountry(String country) {
    this.country = country;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public void setFullOrLegalName(String fullOrLegalName) {
    this.fullOrLegalName = fullOrLegalName;
  }

  public void setKecamatan(String kecamatan) {
    this.kecamatan = kecamatan;
  }

  public void setKelurahan(String kelurahan) {
    this.kelurahan = kelurahan;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  public void setMiddleName(String middleName) {
    this.middleName = middleName;
  }

  public void setPhoneNumber(String phoneNumber) {
    this.phoneNumber = phoneNumber;
  }

  public void setPostalCode(String postalCode) {
    this.postalCode = postalCode;
  }

  public void setPrimary(boolean primary) {
    this.primary = primary;
  }

  public void setState(String state) {
    this.state = state;
  }

  public void setStreetAddress1(String streetAddress1) {
    this.streetAddress1 = streetAddress1;
  }

  public void setStreetAddress2(String streetAddress2) {
    this.streetAddress2 = streetAddress2;
  }

  @Override
  public String toString() {
    return String
        .format(
            "AddressVO [addressId=%s, streetAddress1=%s, streetAddress2=%s, kecamatan=%s, kelurahan=%s, city=%s, state=%s, postalCode=%s, country=%s, firstName=%s, lastName=%s, middleName=%s, fullOrLegalName=%s, phoneNumber=%s, email=%s, addressLabel=%s, primary=%s, toString()=%s]",
            this.addressId, this.streetAddress1, this.streetAddress2, this.kecamatan,
            this.kelurahan, this.city, this.state, this.postalCode, this.country, this.firstName,
            this.lastName, this.middleName, this.fullOrLegalName, this.phoneNumber, this.email,
            this.addressLabel, this.primary, super.toString());
  }
}
