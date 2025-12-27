package com.gdn.partners.pcu.external.web;

import lombok.NoArgsConstructor;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
@NoArgsConstructor
public enum BulkActivityStatusWeb {
  SUCCESS, PARTIAL_SUCCESS, FAILED, IN_PROGRESS, NA, PENDING;
}
