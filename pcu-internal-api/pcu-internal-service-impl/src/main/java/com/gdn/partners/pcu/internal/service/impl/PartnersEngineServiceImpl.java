package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.internal.client.feign.PartnersEngineFeign;
import com.gdn.partners.pcu.internal.client.model.request.UserFilter;
import com.gdn.partners.pcu.internal.client.model.response.UserResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;

import static com.gdn.partners.pcu.internal.model.Constants.ASC_SORT_ORDER;
import static com.gdn.partners.pcu.internal.model.Constants.CREATED_DATE;

@Service
public class PartnersEngineServiceImpl implements PartnersEngineService {

  @Autowired
  private PartnersEngineFeign partnersEngineFeign;

  @Value("${msku.role.code}")
  private String mskuRoleCode;

  private Set<String> findUsernameByFilter(UserFilter filter) throws Exception {
    int page = 0, totalPage = 0;
    long totalElements = 0;
    Set<String> usernames = new HashSet<>();
    do {
      ListBaseResponse<UserResponse> users = this.partnersEngineFeign.filter(page, 100, filter);
      ResponseHelper.validateResponse(users);
      totalElements = users.getMetadata().getTotalItems();
      totalPage = (int) Math.ceil(totalElements / 100.0);
      usernames.addAll(users.getContent().stream().map(UserResponse::getUsername).collect(Collectors.toList()));
      page++;
    } while (page != totalPage);
    return usernames;
  }

  @Override
  public Map<String, List<String>> getReviewers() throws Exception {
    HashMap<String, List<String>> response = new HashMap<>();
    response.put(Constants.REVIEWERS, new ArrayList<>(this.findUsernameByFilter(
      UserFilter.builder().roleCode("VENDOR_VENDOR-REVIEWER").sortedBy(CREATED_DATE)
        .sortDirection(ASC_SORT_ORDER).build())));
    return response;
  }

  @Override
  public List<String> getMasterSkuReviewers() throws Exception {
    return new ArrayList<>(this.findUsernameByFilter(
      UserFilter.builder().roleCode(mskuRoleCode).sortedBy(CREATED_DATE)
        .sortDirection(ASC_SORT_ORDER).build()));
  }

  @Override
  public List<String> getIPRReviewersByRoleCode(String roleCode) throws Exception {
    Set<String> roleCodes = new HashSet<>(Arrays.asList(roleCode.split(Constants.COMMA_SEPARATOR)));
    return new ArrayList<>(this.findUsernameByFilter(
      UserFilter.builder().roleCodes(roleCodes).sortedBy(CREATED_DATE).sortDirection(ASC_SORT_ORDER)
        .build()));
  }
}
