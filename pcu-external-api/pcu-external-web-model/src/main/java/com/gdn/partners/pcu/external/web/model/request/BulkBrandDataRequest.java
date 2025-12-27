package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class BulkBrandDataRequest {
  List<BulkBrandData> downloadRequest;
}
