package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class B2bFieldsRequest implements Serializable {

  private static final long serialVersionUID = 43958386850537543L;
  private Double basePrice;
  private boolean managed;
}
