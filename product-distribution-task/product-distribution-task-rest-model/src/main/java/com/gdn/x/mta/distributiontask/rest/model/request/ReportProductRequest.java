package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ReportProductRequest implements Serializable {

  private static final long serialVersionUID = 5320547357844580534L;
  private String memberId;
  private String itemSku;
  private String reason;
  private String notes;
}
