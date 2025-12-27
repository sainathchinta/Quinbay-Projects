package com.gdn.mta.bulk.repository.campaign;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.XCampaignFeign;
import com.gdn.mta.bulk.models.CampaignProductUpdateRequest;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.x.campaign.request.CampaignProductDetailRequest;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;

public class CampaignRepositoryBeanTest {

  private final static String STORE_ID = "10001";
  private final static String CHANNEL_ID = "api";
  private final static String CLIENT_ID = "x-bulk";
  private final static String REQUEST_ID = "REQUEST-ID";
  private final static String USERNAME = "x-bulk";

  private static final String CAMPAIGN_CODE = "CAMPAIGN-CODE";
  private static final String ITEM_SKU_ID = "ITEM-SKU-ID";
  private static final String PICK_UP_POINT_ID = "PICK-UP-POINT-ID";

  @InjectMocks
  private CampaignRepositoryBean campaignRepositoryBean;

  @Mock
  private XCampaignFeign xCampaignFeign;

  @Test
  public void testGetCampaignProductDetailsV2SuccessTest() throws Exception {
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue = new BulkAddCampaignProductQueue();
    bulkAddCampaignProductQueue.setCampaignCode(CAMPAIGN_CODE);
    bulkAddCampaignProductQueue.setStoreId(STORE_ID);
    bulkAddCampaignProductQueue.setRequestId(REQUEST_ID);
    bulkAddCampaignProductQueue.setUpdatedBy(USERNAME);

    ItemDetailsDto itemDetailsDto = new ItemDetailsDto();
    itemDetailsDto.setItemSku(ITEM_SKU_ID);
    itemDetailsDto.setPickUpPointCode(PICK_UP_POINT_ID);
    List<ItemDetailsDto> itemSkuList = new ArrayList<>();
    itemSkuList.add(itemDetailsDto);

    GdnRestListResponse<CampaignProductDetailResponse>
      campaignProductDetailResponseGdnRestListResponse =
      new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
        new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(xCampaignFeign
      .getCampaignProductDetailsV2(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(CampaignProductDetailRequest.class)))
      .thenReturn(campaignProductDetailResponseGdnRestListResponse);

    this.campaignRepositoryBean
      .getCampaignProductDetailsV2(itemSkuList, bulkAddCampaignProductQueue);

    Mockito.verify(xCampaignFeign)
      .getCampaignProductDetailsV2(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(CampaignProductDetailRequest.class));

  }

  @Test
  public void testGetCampaignProductDetailsV2FailedTest() throws Exception {
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue = new BulkAddCampaignProductQueue();
    bulkAddCampaignProductQueue.setCampaignCode(CAMPAIGN_CODE);
    bulkAddCampaignProductQueue.setStoreId(STORE_ID);
    bulkAddCampaignProductQueue.setRequestId(REQUEST_ID);
    bulkAddCampaignProductQueue.setUpdatedBy(USERNAME);

    ItemDetailsDto itemDetailsDto = new ItemDetailsDto();
    itemDetailsDto.setItemSku(ITEM_SKU_ID);
    itemDetailsDto.setPickUpPointCode(PICK_UP_POINT_ID);
    List<ItemDetailsDto> itemSkuList = new ArrayList<>();
    itemSkuList.add(itemDetailsDto);

    GdnRestListResponse<CampaignProductDetailResponse>
      campaignProductDetailResponseGdnRestListResponse =
      new GdnRestListResponse<>(null, null, false, Collections.emptyList(),
        new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(xCampaignFeign
      .getCampaignProductDetailsV2(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(CampaignProductDetailRequest.class)))
      .thenReturn(campaignProductDetailResponseGdnRestListResponse);

    try {
      Assertions.assertThrows(RuntimeException.class, () -> this.campaignRepositoryBean
        .getCampaignProductDetailsV2(itemSkuList, bulkAddCampaignProductQueue));
    } finally {
      Mockito.verify(xCampaignFeign)
        .getCampaignProductDetailsV2(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
          Mockito.any(CampaignProductDetailRequest.class));
    }
  }

  @Test
  public void updateCampaignFinalPriceAndQuotaTest() {
    Mockito.when(xCampaignFeign.updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of())))
        .thenReturn(new GdnRestSingleResponse<>(new CampaignUpdateResponse(), Constant.REQUEST_ID));
    campaignRepositoryBean.updateCampaignFinalPriceAndQuota(List.of(), CAMPAIGN_CODE);
    Mockito.verify(xCampaignFeign)
        .updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of()));
  }

  @Test
  public void updateCampaignFinalPriceAndQuota_successFalseTest() {
    boolean isSuccess = true;
    ErrorCategory errorCategory = null;
    Mockito.when(xCampaignFeign.updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of())))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new CampaignUpdateResponse(), Constant.REQUEST_ID));
    try {
      campaignRepositoryBean.updateCampaignFinalPriceAndQuota(List.of(), CAMPAIGN_CODE);
    } catch (ApplicationRuntimeException arx) {
      isSuccess = false;
      errorCategory = arx.getErrorCodes();
    } finally {
      Mockito.verify(xCampaignFeign)
        .updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of()));
      Assertions.assertFalse(isSuccess);
      Assertions.assertEquals(ErrorCategory.UNSPECIFIED, errorCategory);
    }
  }

  @Test
  public void updateCampaignFinalPriceAndQuota_exceptionTest() {
    boolean isSuccess = true;
    ErrorCategory errorCategory = null;
    Mockito.when(xCampaignFeign.updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of()))).thenThrow(ApplicationRuntimeException.class);
    try {
      campaignRepositoryBean.updateCampaignFinalPriceAndQuota(List.of(), CAMPAIGN_CODE);
    } catch (ApplicationRuntimeException arx) {
      isSuccess = false;
      errorCategory = arx.getErrorCodes();
    } finally {
      Mockito.verify(xCampaignFeign)
        .updateCampaignFinalPriceAndQuota(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new CampaignProductUpdateRequest(CAMPAIGN_CODE, List.of()));
      Assertions.assertFalse(isSuccess);
      Assertions.assertEquals(ErrorCategory.UNSPECIFIED, errorCategory);
    }
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.xCampaignFeign);
  }

}
