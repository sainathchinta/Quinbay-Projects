package com.gdn.mta.bulk.models;

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
public class IPRProductsUploadEmailTemplate {
  private String emailTemplateId;
  private String emailTemplate;
  private Integer successCount;
  private Integer totalCount;
  private Integer failureCount;
}
