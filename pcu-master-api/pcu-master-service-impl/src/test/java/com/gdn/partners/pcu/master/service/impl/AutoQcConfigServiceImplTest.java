package com.gdn.partners.pcu.master.service.impl;

import java.util.ArrayList;
import java.util.UUID;

import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.master.client.feign.PBPFeign;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;

public class AutoQcConfigServiceImplTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_RULE_NAME = "OFFICIAL_SELLERS";
  private static final String RULE_NAME = "OFFICIAL_SELLERS";
  private static final String AUTO_APPROVAL_TYPE = "CONTENT_AND_IMAGE";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private AutoQcConfigServiceImpl autoQcConfigService;

  @Mock
  private PBPFeign pbpFeign;

  private AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest;
  private AutoApprovalRulesListResponse autoApprovalRulesListResponse;
  private AutoApprovalRulesDto autoApprovalRulesDto;
  private GdnRestSimpleResponse<AutoApprovalRulesListResponse> gdnRestSimpleResponse;

  @BeforeEach
  void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    autoQcConfigUpdateWebRequest = new AutoQcConfigUpdateWebRequest(new ArrayList<>(), Boolean.TRUE);
    autoApprovalRulesDto = new AutoApprovalRulesDto();
    autoApprovalRulesDto.setRuleName(RULE_NAME);
    autoApprovalRulesDto.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    autoApprovalRulesDto.setRuleConfig(new ArrayList<>());
    autoApprovalRulesDto.setImageQcConfig(new ArrayList<>());
    autoApprovalRulesDto.setNeedRevisionImageQcConfig(new ArrayList<>());
    autoApprovalRulesListResponse = new AutoApprovalRulesListResponse();
    autoApprovalRulesListResponse.setAutoApprovalRulesDtoList(Arrays.asList(autoApprovalRulesDto));
    gdnRestSimpleResponse = new GdnRestSimpleResponse<>(null, null, true, REQUEST_ID, autoApprovalRulesListResponse);
  }

  @Test
  void updateAutoQcConfigRuleTest() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_AUTO_QC_CONFIG;
    Credential.setAccessibilities(accessibilities);

    Mockito.when(
        pbpFeign.update(false, DEFAULT_RULE_NAME, RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID));

    GdnBaseRestResponse gdnBaseRestResponse =
        autoQcConfigService.updateAutoQcConfigRule(DEFAULT_RULE_NAME, false, autoQcConfigUpdateWebRequest);
    Assertions.assertTrue(gdnBaseRestResponse.isSuccess());
    Mockito.verify(pbpFeign)
        .update(false, DEFAULT_RULE_NAME, RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest));
  }

  @Test
  void updateAutoQcConfigRuleTest_CheckAccessibility() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = "";
    Credential.setAccessibilities(accessibilities);
    Mockito.when(
        pbpFeign.update(false, DEFAULT_RULE_NAME, RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID));

    try {
      GdnBaseRestResponse gdnBaseRestResponse =
          autoQcConfigService.updateAutoQcConfigRule(DEFAULT_RULE_NAME, false, autoQcConfigUpdateWebRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE));
    }
  }

  @Test
  void getAutoApprovalRulesTest() throws Exception {
    Mockito.when(pbpFeign.getAutoApprovalRules()).thenReturn(gdnRestSimpleResponse);
    AutoApprovalRulesListWebResponse response = autoQcConfigService.getAutoApprovalRules();
    Mockito.verify(pbpFeign).getAutoApprovalRules();
    Assertions.assertEquals(RULE_NAME,
        response.getAutoApprovalRulesWebResponseList().get(0).getRuleName());
    Assertions.assertEquals(AUTO_APPROVAL_TYPE,
        response.getAutoApprovalRulesWebResponseList().get(0).getAutoApprovalType());
  }
}
