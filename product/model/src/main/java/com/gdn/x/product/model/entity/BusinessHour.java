package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.DayOfWeek;
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
public class BusinessHour implements GdnBaseEmbedded {

  private static final long serialVersionUID = 2627608241592178754L;

  @Field(value = "day")
  private DayOfWeek day;

  @Field(value = "openingTimeInSeconds")
  private int openingTimeInSeconds;

  @Field(value = "closingTimeInSeconds")
  private int closingTimeInSeconds;

  @Field(value = "open")
  private boolean open;
}
