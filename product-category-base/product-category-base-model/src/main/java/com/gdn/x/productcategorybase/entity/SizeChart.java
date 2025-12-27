package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@Table(name = SizeChart.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {SizeChart.COLUMN_SIZE_CHART_BUSINESS_PARTNER_CODE,
        SizeChart.COLUMN_SIZE_CHART_NAME})})
@ToString()
public class SizeChart extends GdnBaseEntity {

  private static final long serialVersionUID= 5799467834690155088L;

  public static final String TABLE_NAME = "PCC_SIZE_CHART";
  public static final String COLUMN_SIZE_CHART_CODE = "SIZE_CHART_CODE";
  public static final String COLUMN_SIZE_CHART_NAME = "SIZE_CHART_NAME";
  public static final String COLUMN_SIZE_CHART_BRAND = "BRAND";
  public static final String COLUMN_SIZE_CHART_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_SIZE_CHART_SIZE_ATTRIBUTE_CODE = "SIZE_ATTRIBUTE_CODE";
  public static final String COLUMN_SIZE_CHART_SIZE_ATTRIBUTE_NAME = "SIZE_ATTRIBUTE_NAME";
  public static final String COLUMN_SIZE_CHART_SELECTED_SIZE_ATTRIBUTE_VALUE_TYPES = "SELECTED_SIZE_ATTRIBUTE_VALUE_TYPES";
  public static final String COLUMN_SIZE_CHART_SELECTED_DIMENSIONS = "SELECTED_DIMENSIONS";
  public static final String COLUMN_SIZE_CHART_GENDER = "GENDER";
  public static final String COLUMN_SIZE_CHART_UNIT = "UNIT";
  public static final String COLUMN_SIZE_CHART_FIT_TIP = "FIT_TIP";
  public static final String COLUMN_SIZE_CHART_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_SIZE_CHART_ROWS = "SIZE_CHART_ROWS";
  public static final String COLUMN_WAITING_DELETION = "WAITING_DELETION";

  @Column(name = SizeChart.COLUMN_SIZE_CHART_CODE)
  private String sizeChartCode;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_NAME)
  private String name;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_BRAND)
  private String brand;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_BRAND_CODE)
  private String brandCode;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_SIZE_ATTRIBUTE_CODE)
  private String sizeAttributeCode;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_SIZE_ATTRIBUTE_NAME)
  private String sizeAttributeName;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_SELECTED_SIZE_ATTRIBUTE_VALUE_TYPES)
  private String sizeAttributeValueTypes;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_SELECTED_DIMENSIONS)
  private String dimensions;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_GENDER)
  private String gender;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_UNIT)
  private String unit;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_FIT_TIP)
  private String fitTip;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = SizeChart.COLUMN_SIZE_CHART_ROWS)
  private String sizeChartRows;

  @Column(name = SizeChart.COLUMN_WAITING_DELETION)
  private boolean waitingDeletion;
}