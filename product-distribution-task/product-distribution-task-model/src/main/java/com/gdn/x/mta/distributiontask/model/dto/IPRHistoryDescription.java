package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRHistoryDescription implements Serializable {
  private String previous;
  private String current;
}
