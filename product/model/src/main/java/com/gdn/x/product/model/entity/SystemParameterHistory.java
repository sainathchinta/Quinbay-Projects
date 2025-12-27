package com.gdn.x.product.model.entity;

import org.springframework.beans.BeanUtils;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

@Document(collection = ProductFieldNames.SYSTEM_PARAMETER_HISTORY)
public class SystemParameterHistory extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.VARIABLE)
  private String variable;

  @Field(value = ProductFieldNames.VALUE)
  private String value;

  @Field(value = ProductFieldNames.DESCRIPTION)
  private String description;

  public SystemParameterHistory() {
    // do nothing
  }

  /**
   * @param storeId
   * @param variable
   * @param value
   * @param description
   */
  public SystemParameterHistory(String storeId, String variable, String value, String description) {
    super.setStoreId(storeId);
    this.variable = variable;
    this.value = value;
    this.description = description;
  }

  /**
   *
   * @param systemParameter
   */
  public SystemParameterHistory(SystemParameter systemParameter) {
    BeanUtils.copyProperties(systemParameter, this);
    super.setId(null);
    super.setVersion(null);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  /**
   * @return the description
   */
  public String getDescription() {
    return description;
  }


  /**
   * @return the value
   */
  public String getValue() {
    return this.value;
  }

  /**
   * @return the variable
   */
  public String getVariable() {
    return this.variable;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  /**
   * @param description the description to set
   */
  public void setDescription(String description) {
    this.description = description;
  }


  /**
   * @param value the value to set
   */
  public void setValue(String value) {
    this.value = value;
  }



  /**
   * @param variable the variable to set
   */
  public void setVariable(String variable) {
    this.variable = variable;
  }

  /*
   * (non-Javadoc)
   *
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.format(
        "SystemParameterHistory [variable=%s, value=%s, description=%s, toString()=%s]", variable,
        value, description, super.toString());
  }



}
