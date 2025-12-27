package com.gdn.partners.pbp.dto.productlevel3;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PostLiveProductCountResponse extends BaseResponse {
  private Integer postLiveProductCount;
  private Map<String, NeedRevisionDeletionStatus> needRevisionDeletionStatus;
}
