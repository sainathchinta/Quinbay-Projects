package com.gda.mta.product.dto;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CogsUpdateRequests extends BaseRequest implements Serializable {

  @Serial
  private static final long serialVersionUID = 3150398992461161352L;
  
  private List<CogsUpdateDtoRequest> listRequest;
}
