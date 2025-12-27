package com.gdn.partners.pcu.external.web.model.request;


import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class B2bFieldsWebRequest implements Serializable {
  private static final long serialVersionUID = 5076247251188012614L;

  private Double price;
  private boolean buyable;
  private boolean display;
  private boolean managed;
}
