package com.gdn.partners.pcu.internal.client.model.response;


import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BPJPHListResponse<T> implements Serializable {

  private static final long serialVersionUID = 7773322705462739428L;

  private int statusCode;
  private String message;
  private boolean error;
  private BPJPHData data;
}
