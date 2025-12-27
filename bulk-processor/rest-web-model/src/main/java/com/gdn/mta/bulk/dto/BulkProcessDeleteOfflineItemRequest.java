package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessDeleteOfflineItemRequest extends BulkProcessUpdateRequest {

  private static final long serialVersionUID = 8834610293068216473L;
  private Set<String> accessiblePickupPoints = new HashSet<>();
}
