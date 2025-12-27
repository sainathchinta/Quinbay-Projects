package com.gdn.mta.bulk.dto;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessUpsertOfflineItemRequest extends BulkProcessUpdateRequest {

  private static final long serialVersionUID = -2280842090216854496L;

  private Set<String> accessiblePickupPoints = new HashSet<>();
}
