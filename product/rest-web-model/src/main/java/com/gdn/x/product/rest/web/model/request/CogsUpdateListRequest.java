package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.List;

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
public class CogsUpdateListRequest implements Serializable {

  private static final long serialVersionUID= -1950986330385575472L;
  List<CogsUpdateRequest> listRequest;
}
