package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.mta.bulk.feignConfig.PartnersEngineFeign;
import com.gdn.mta.bulk.request.UserFilter;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.core.web.dto.ListBaseResponse;

public class PartnersEngineOutboundServiceImplTest {

  private static final String ROLE_CODE = "code";
  private static final String SORT = "sort";
  private static final String DIRECTION = "direction";

  @InjectMocks
  private PartnersEngineOutboundServiceImpl partnersEngineOutboundServiceImpl;

  @Mock
  private PartnersEngineFeign partnersEngineFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(partnersEngineOutboundServiceImpl, "partnerEngineSize", 100);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(partnersEngineFeign);
  }

  @Test
  public void userFilterTest() {
    Mockito.when(partnersEngineFeign
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build()))
        .thenReturn(null);
    ListBaseResponse<UserResponse> response =
        partnersEngineOutboundServiceImpl.userFilter(ROLE_CODE, SORT, DIRECTION, 1, null);
    Mockito.verify(partnersEngineFeign)
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build());
    Assertions.assertNull(response);
  }

  @Test
  public void userFilter_iprProductsReviewers() {
    Set<String> iprReviewersRoleCode = new HashSet<>(Arrays.asList(new String[] {ROLE_CODE}));
    Mockito.when(partnersEngineFeign
            .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
                1, 100, UserFilter.builder().roleCodes(iprReviewersRoleCode).sortedBy(SORT).sortDirection(DIRECTION).build()))
        .thenReturn(null);
    ListBaseResponse<UserResponse> response =
        partnersEngineOutboundServiceImpl.userFilter(null, SORT, DIRECTION, 1, iprReviewersRoleCode);
    Mockito.verify(partnersEngineFeign)
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCodes(iprReviewersRoleCode).sortedBy(SORT).sortDirection(DIRECTION).build());
    Assertions.assertNull(response);
  }

  @Test
  public void userFilterSuccessFalse() {
    Mockito.when(partnersEngineFeign
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build()))
        .thenReturn(new ListBaseResponse<>(null, null, false, null, null, null));
    ListBaseResponse<UserResponse> response =
        partnersEngineOutboundServiceImpl.userFilter(ROLE_CODE, SORT, DIRECTION, 1, null);
    Mockito.verify(partnersEngineFeign)
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build());
    Assertions.assertNull(response);
  }

  @Test
  public void userFilterSuccess() {
    Mockito.when(partnersEngineFeign
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build()))
        .thenReturn(new ListBaseResponse<>(null, null, true, null, new ArrayList<>(), null));
    ListBaseResponse<UserResponse> response =
        partnersEngineOutboundServiceImpl.userFilter(ROLE_CODE, SORT, DIRECTION, 1, null);
    Mockito.verify(partnersEngineFeign)
        .userFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            1, 100, UserFilter.builder().roleCode(ROLE_CODE).sortedBy(SORT).sortDirection(DIRECTION).build());
    Assertions.assertNotNull(response);
  }
}