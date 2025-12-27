package com.gdn.mta.product.entity;

/**
 * Created by Kesha on 11/04/16.
 */
public class ProductFieldHistory {

 private String fieldName;
 private Object oldValue;
 private Object newValue;

 public ProductFieldHistory(String fieldName, Object oldValue, Object newValue) {
  this.fieldName = fieldName;
  this.oldValue = oldValue;
  this.newValue = newValue;
 }

 public String getFieldName() {
  return fieldName;
 }

 public Object getOldValue() {
  return oldValue;
 }

 public Object getNewValue() {
  return newValue;
 }

 @Override
 public String toString() {
  final StringBuilder sb = new StringBuilder("{");
  sb.append("field: '").append(fieldName).append('\'');
  sb.append(", oldValue: ").append(oldValue);
  sb.append(", newValue: ").append(newValue);
  sb.append('}');
  return sb.toString();
 }
}
