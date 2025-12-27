package com.gdn.partners.pbp.model.productlevel3;

import java.util.Date;

import org.codehaus.jackson.annotate.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BuyableScheduleDTO {

  private boolean buyable;
  private Date startDateTime;
  private Date endDateTime;
}
