package com.gdn.mta.bulk.service;

import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.mta.bulk.feignConfig.PartnersEngineFeign;
import com.gdn.mta.bulk.request.UserFilter;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PartnersEngineOutboundServiceImpl implements PartnersEngineOutboundService {

  @Autowired
  private PartnersEngineFeign partnersEngineFeign;

  @Value("${partners.engine.size}")
  private int partnerEngineSize;

  @Override
  public ListBaseResponse<UserResponse> userFilter(String roleCode, String sortedBy,
      String sortDirection, int page, Set<String> roleCodes) {
    ListBaseResponse<UserResponse> response = null;
    if (CollectionUtils.isNotEmpty(roleCodes)) {
      response = partnersEngineFeign.userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, page, partnerEngineSize,
          UserFilter.builder().roleCodes(roleCodes).sortedBy(sortedBy).sortDirection(sortDirection)
              .build());
    }
    else {
      response = partnersEngineFeign.userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, page, partnerEngineSize,
          UserFilter.builder().roleCode(roleCode).sortedBy(sortedBy).sortDirection(sortDirection)
              .build());
    }
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Exception while getting user data from partners engine error ");
      return null;
    }
    return response;
  }

}