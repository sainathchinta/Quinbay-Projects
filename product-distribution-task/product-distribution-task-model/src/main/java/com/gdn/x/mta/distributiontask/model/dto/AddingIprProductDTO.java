package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AddingIprProductDTO {
  IPRProductSolr iprProductSolr;
  ErrorCategory errorCategory;
  boolean onlyAssigneeUpdated;
  String existingAssignee;
}