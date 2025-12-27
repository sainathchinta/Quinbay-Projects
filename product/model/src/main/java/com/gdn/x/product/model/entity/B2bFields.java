package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class B2bFields implements GdnBaseEmbedded {

  private static final long serialVersionUID = -4336774942821833334L;

  @Field(value = ProductFieldNames.MANAGED)
  private boolean managed;

  @Field(value = ProductFieldNames.BASE_PRICE)
  private Double basePrice;
}
