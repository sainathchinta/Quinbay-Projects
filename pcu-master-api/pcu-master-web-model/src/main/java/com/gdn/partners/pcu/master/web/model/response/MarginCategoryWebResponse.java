package com.gdn.partners.pcu.master.web.model.response;

import java.util.Date;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/01/2019 AD.
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class MarginCategoryWebResponse {
  private String categoryCode;
  private Double transactionFee;
  private Double value;
  private Double minimumValue;
  private Double maximumValue;
  private String note;
  private Date startDate;
  private Date endDate;
  private String valueType;
  private String marginType;
}
