package com.gdn.x.mta.distributiontask.rest.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude
@Builder
public class RestrictedKeywordsByFieldVendor {

  private String fieldIdentifier;
  private List<String> keywords;
}
