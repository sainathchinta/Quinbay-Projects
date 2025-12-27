package com.gdn.partners.pcu.internal.service.impl;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LookupServiceImplTest {

  private static final String LOOKUP_GROUP = "DANGEROUS_GOODS_LEVEL";
  private static final String ID = "1";
  private static final String NAME = "Name";
  private static final String REQUEST_ID = "requestId";

  private LookupResponse lookupResponse;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private LookupServiceImpl lookupService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    lookupResponse = new LookupResponse();
    lookupResponse.setLookupGroup(LOOKUP_GROUP);
    lookupResponse.setId(ID);
    lookupResponse.setName(NAME);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  @Test
  public void getDangerousGoodsLevelTest() {
    Mockito.when(this.pcbFeign.getLookupByLookupGroup(LOOKUP_GROUP))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(lookupResponse), new PageMetaData(), REQUEST_ID));
    List<LookupWebResponse> responseList = this.lookupService.getDangerousGoodsLevel();
    Mockito.verify(this.pcbFeign).getLookupByLookupGroup(LOOKUP_GROUP);
    Assertions.assertEquals(1, responseList.size());
    Assertions.assertEquals(NAME, responseList.get(0).getName());
    Assertions.assertEquals(ID, responseList.get(0).getId());
    Assertions.assertEquals(LOOKUP_GROUP, responseList.get(0).getLookupGroup());
  }

}
