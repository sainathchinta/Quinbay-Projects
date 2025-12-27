package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class Geolocation implements GdnBaseEmbedded {

  private static final long serialVersionUID = 2627608241592178754L;

  @Field(value = "placeId")
  private String placeId;

  @Field(value = "latitude")
  private Double latitude;

  @Field(value = "longitude")
  private Double longitude;

  @Field(value = "streetAddress")
  private String streetAddress;
}
