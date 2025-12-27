package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper=true)
@Entity
@Table(name = Lookup.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {Lookup.COLUMN_LOOKUP_GROUP, Lookup.COLUMN_CODE})})
public class Lookup extends GdnBaseEntity {

  private static final long serialVersionUID = -3309651783631469696L;

  public static final String TABLE_NAME = "PCC_LOOKUP";
  public static final String COLUMN_CODE = "CODE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_LOOKUP_GROUP = "LOOKUP_GROUP";
  public static final String COLUMN_ORDER_NUMBER = "ORDER_NO";
  public static final String COLUMN_NAME = "NAME";

  @Column(name = Lookup.COLUMN_CODE)
  private String code;

  @Column(name = Lookup.COLUMN_DESCRIPTION)
  private String description;

  @Column(name = Lookup.COLUMN_LOOKUP_GROUP)
  private String lookupGroup;

  @Column(name = Lookup.COLUMN_ORDER_NUMBER)
  private Integer orderNumber;

  @Column(name = Lookup.COLUMN_NAME)
  private String name;

  @Override
  public String toString() {
    return String.format("Lookup [code=%s, lookupGroup=%s, name=%s, orderNumber=%s, description=%s]", this.code,
        this.lookupGroup, this.name, this.orderNumber, this.description, super.toString());
  }
}
