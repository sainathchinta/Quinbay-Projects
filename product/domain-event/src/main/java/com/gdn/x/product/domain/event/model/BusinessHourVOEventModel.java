package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.DayOfWeek;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BusinessHourVOEventModel {
  private DayOfWeek day;
  private int openingTimeInSeconds;
  private int closingTimeInSeconds;
  private boolean open;
}
