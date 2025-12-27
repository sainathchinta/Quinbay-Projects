package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@EqualsAndHashCode(callSuper = false)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = PickupPoint.DOCUMENT_NAME)
public class PickupPoint extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "pickup_point";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Field(value = ProductFieldNames.CNC_ACTIVATED)
  private boolean cncActivated;

}
