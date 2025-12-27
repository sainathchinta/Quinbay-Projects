package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = DimensionMapping.TABLE_NAME)
@ToString()
public class DimensionMapping extends GdnBaseEntity {

  private static final long serialVersionUID = 4618291098449550456L;

  public static final String TABLE_NAME = "PCC_DIMENSION_MAPPING";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  public static final String COLUMN_MANDATORY = "MANDATORY";
  public static final String COLUMN_DIMENSION_ID = "DIMENSION_ID";

  @Column(name = DimensionMapping.COLUMN_ATTRIBUTE_ID)
  private String attributeId;

  @Column(name = DimensionMapping.COLUMN_ATTRIBUTE_CODE)
  private String attributeCode;

  @ManyToOne
  @JoinColumn(name = DimensionMapping.COLUMN_DIMENSION_ID)
  private Dimension dimension;

  @Column(name = DimensionMapping.COLUMN_MANDATORY)
  private boolean mandatory = false;

}
