package com.gdn.mta.product.entity;

import com.gdn.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductDataAutoFixHistory.TABLE_NAME)
public class ProductDataAutoFixHistory extends GdnBaseEntity {
  private static final long serialVersionUID = 2361897705894641980L;
  public static final String TABLE_NAME = "PRD_PRODUCT_DATA_AUTO_FIX_HISTORY";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_TYPE = "TYPE";
  public static final String COLUMN_ADDITIONAL_INFO = "ADDITIONAL_INFO";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_TYPE, nullable = false)
  private String type;

  @Column(name = COLUMN_ADDITIONAL_INFO)
  private String additionalInfo;
}
