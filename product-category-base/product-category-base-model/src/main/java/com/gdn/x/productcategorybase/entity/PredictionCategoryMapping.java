package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Data
@Table(name = PredictionCategoryMapping.TABLE_NAME)
public class PredictionCategoryMapping extends GdnBaseEntity {

  private static final long serialVersionUID = -6714415199144075353L;
  public static final String TABLE_NAME = "PCC_PREDICTION_CATEGORY_MAPPING";
  public static final String COLUMN_PREDICTION_ID = "PREDICTION_ID";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";

  @Column(name = PredictionCategoryMapping.COLUMN_PREDICTION_ID)
  private String predictionId;

  @Column(name = PredictionCategoryMapping.COLUMN_CATEGORY_CODE)
  private String categoryCode;

}
