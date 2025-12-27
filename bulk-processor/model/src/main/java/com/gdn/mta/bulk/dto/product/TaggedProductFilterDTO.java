package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class TaggedProductFilterDTO extends BulkDownloadRequest {
  private static final long serialVersionUID = 7667998391592802761L;

  private List<String> businessPartnerCodes;
  private List<String> categoryCodes;
  private List<String> productTypes;
  private List<String> itemSkus;
  private String emailAddress;
}
