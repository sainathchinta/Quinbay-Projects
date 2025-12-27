package com.gdn.mta.bulk.dto;

import java.util.HashSet;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BulkProcessV2Request extends BulkProcessUpdateRequest {

  private Set<String> accessiblePickupPoints = new HashSet<>();
}
