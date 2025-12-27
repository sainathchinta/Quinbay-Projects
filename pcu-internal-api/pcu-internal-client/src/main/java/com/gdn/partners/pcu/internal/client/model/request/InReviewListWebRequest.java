package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Navya Naveli
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
public class InReviewListWebRequest {
  private String assignedTo;
  private String keyword;
  private String categoryCode;
  private long startDate;
  private long endDate;
}
