package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@Entity
@Table(name = ProductAttributeExtracted.TABLE_NAME)
public class ProductAttributeExtracted extends GdnBaseEntity {
  private static final long serialVersionUID = 5560699758471442022L;

  public static final String TABLE_NAME = "PCC_PRODUCT_ATTRIBUTE_EXTRACTED";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_CN_CATEGORY_CODE = "CN_CATEGORY_CODE";
  public static final String COLUMN_EXTRACTED_ATTRIBUTE = "EXTRACTED_ATTRIBUTE";
  public static final String COLUMN_SOURCE = "SOURCE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";

  @Column(name = ProductAttributeExtracted.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = ProductAttributeExtracted.COLUMN_CN_CATEGORY_CODE)
  private String cnCategoryCode;

  @Column(name = ProductAttributeExtracted.COLUMN_EXTRACTED_ATTRIBUTE)
  private String extractedAttribute;

  @Column(name = ProductAttributeExtracted.COLUMN_SOURCE)
  private String source;

  @Column(name = ProductAttributeExtracted.COLUMN_STATUS)
  @Enumerated(value = EnumType.STRING)
  private ExtractionStatus status;

  @Column(name = ProductAttributeExtracted.COLUMN_ERROR_MESSAGE)
  private String errorMessage;

}
