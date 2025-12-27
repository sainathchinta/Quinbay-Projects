package com.gda.mta.product.dto;


import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class NeedRevisionProductsRequest implements Serializable {

  private static final long serialVersionUID = 543797027847503434L;
  private String state;
  private Date startUpdatedDate;
  private Date endUpdatedDate;
}
