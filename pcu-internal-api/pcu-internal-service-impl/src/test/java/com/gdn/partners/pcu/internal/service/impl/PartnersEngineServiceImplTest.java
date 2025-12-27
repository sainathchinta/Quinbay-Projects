package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.feign.PartnersEngineFeign;
import com.gdn.partners.pcu.internal.client.model.request.UserFilter;
import com.gdn.partners.pcu.internal.client.model.response.UserResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class PartnersEngineServiceImplTest {

  private List<UserResponse> userResponses = new ArrayList<>();
  private UserResponse userResponse = new UserResponse();
  private UserFilter userFilter = new UserFilter();
  private Set<String> stringSet = new HashSet<>();

  private static final String EMAIL = "email";
  private static final String NAME = "name";
  private static final String USERNAME = "username";
  private static final String PHONE = "phone";
  private static final String STATE = "state";
  private static final String STORE_ID = "10001";
  private static final String CREATED_BY = "created-by";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String MSKU_ROLE = "MSKU_ROLE";
  private static final String IPR_ROLE_REVIEWER = "IPR_ROLE_REVIEWER";
  private static final String IPR_ROLE_SUPERVISOR = "IPR_ROLE_SUPERVISOR";
  private static final Date CREATED_DATE = new Date();

  @InjectMocks
  private PartnersEngineServiceImpl partnersEngineService;

  @Mock
  private PartnersEngineFeign partnersEngineFeign;

  @BeforeEach
  public void setUp() throws Exception {
    userResponse =
        UserResponse.builder().name(NAME).roleCodes(stringSet).categoryCodes(stringSet).createdDate(CREATED_DATE)
            .createdBy(CREATED_BY).email(EMAIL).phoneNumber(PHONE).state(STATE).storeId(STORE_ID).salesLevel(STATE)
            .vendorCodes(stringSet).updatedBy(CREATED_BY).updatedDate(CREATED_DATE).username(NAME).build();
    userFilter = UserFilter.builder().categoryCode(CATEGORY_CODE).email(EMAIL).keyword(NAME).mode(STATE).name(NAME)
        .phoneNumber(PHONE).roleCode(STATE).salesLevel(STATE).storeCode(STORE_ID).username(USERNAME)
        .vendorCode(USERNAME).exclusions(new LinkedHashMap<>()).sortDirection("des").sortedBy("des").build();
    userResponses.add(userResponse);
  }

  @Test
  public void getImageAndContentReviewersTest() throws Exception {
    Metadata metadata = new Metadata(0, 100, (long) 1);
    ListBaseResponse<UserResponse> userResponseList =
        new ListBaseResponse<>(null, null, true, null, userResponses, metadata);
    Mockito.when(partnersEngineFeign.filter(Mockito.eq(0), Mockito.anyInt(), Mockito.any(UserFilter.class)))
        .thenReturn(userResponseList);
    Map<String, List<String>> reviewerUsernames = this.partnersEngineService.getReviewers();
    Assertions.assertEquals(1, reviewerUsernames.get(Constants.REVIEWERS).size());
    Mockito.verify(partnersEngineFeign, Mockito.times(1))
        .filter(Mockito.anyInt(), Mockito.anyInt(), Mockito.any(UserFilter.class));
  }

  @Test
  public void getMasterSkuReviewersTest() throws Exception {
    Metadata metadata = new Metadata(0, 100, (long) 1);
    ListBaseResponse<UserResponse> userResponseList =
      new ListBaseResponse<>(null, null, true, null, userResponses, metadata);
    Mockito.when(partnersEngineFeign.filter(Mockito.eq(0), Mockito.anyInt(), Mockito.any(UserFilter.class)))
      .thenReturn(userResponseList);
    List<String> reviewerUsernames = this.partnersEngineService.getMasterSkuReviewers();
    Assertions.assertEquals(1, reviewerUsernames.size());
    Mockito.verify(partnersEngineFeign, Mockito.times(1))
      .filter(Mockito.anyInt(), Mockito.anyInt(), Mockito.any(UserFilter.class));
  }

  @Test
  public void getIPRReviewersTest() throws Exception {
    Metadata metadata = new Metadata(0, 100, (long) 1);
    ListBaseResponse<UserResponse> userResponseList =
      new ListBaseResponse<>(null, null, true, null, userResponses, metadata);
    Mockito.when(
        partnersEngineFeign.filter(Mockito.eq(0), Mockito.anyInt(), Mockito.any(UserFilter.class)))
      .thenReturn(userResponseList);
    List<String> reviewerUsernames = this.partnersEngineService.getIPRReviewersByRoleCode(IPR_ROLE_REVIEWER);
    Assertions.assertEquals(1, reviewerUsernames.size());
    Mockito.verify(partnersEngineFeign, Mockito.times(1))
      .filter(Mockito.anyInt(), Mockito.anyInt(), Mockito.any(UserFilter.class));
  }

  @Test
  public void getIPRReviewersTest_supervisor() throws Exception {
    Metadata metadata = new Metadata(0, 100, (long) 1);
    ListBaseResponse<UserResponse> userResponseList =
        new ListBaseResponse<>(null, null, true, null, userResponses, metadata);
    Mockito.when(
            partnersEngineFeign.filter(Mockito.eq(0), Mockito.anyInt(), Mockito.any(UserFilter.class)))
        .thenReturn(userResponseList);
    List<String> reviewerUsernames = this.partnersEngineService.getIPRReviewersByRoleCode(IPR_ROLE_SUPERVISOR);
    Assertions.assertEquals(1, reviewerUsernames.size());
    Mockito.verify(partnersEngineFeign, Mockito.times(1))
        .filter(Mockito.anyInt(), Mockito.anyInt(), Mockito.any(UserFilter.class));
  }
}
