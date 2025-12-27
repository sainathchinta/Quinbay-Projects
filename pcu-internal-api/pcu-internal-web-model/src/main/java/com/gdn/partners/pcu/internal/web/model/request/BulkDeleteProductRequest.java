package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BulkDeleteProductRequest implements Serializable {
  private List<RejectProductRequest> codes = new ArrayList<>();
  private String notes;
  private RejectReason rejectReason;
}
