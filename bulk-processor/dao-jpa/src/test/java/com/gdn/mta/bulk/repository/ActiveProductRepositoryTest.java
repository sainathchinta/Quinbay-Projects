package com.gdn.mta.bulk.repository;

import com.gda.mta.product.dto.ProductCodesResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSelectedProductDownloadRequest;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class ActiveProductRepositoryTest {

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private ActiveProductsRepositoryBean activeProductRepository;

  GdnRestSingleResponse<ProductCodesResponse> productCodesResponse;
  GdnRestListResponse<MasterProductResponse> masterProductResponse;
  GdnRestListResponse<MasterProductResponse> masterProductResponse1;
  List<String> productCodes = new ArrayList<>();
  private static final String PRODUCT_CODE = "MTA-0001";
  private static final Integer PRODUCT_SIZE = 123;
  private static final String NAME = "name";
  private static final String USERNAME = "name";
  private static final String CATEGORY = "category";
  private static final String REQUEST_ID = "123";
  private static final String KEYWORD = "keyword";
  private static final String SORTBY = "desc";
  private static final String CLIENT_ID = "x-bulk";
  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "api";

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    productCodesResponse = new GdnRestSingleResponse<>();
    ProductCodesResponse codesResponse = new ProductCodesResponse();
    productCodes.add(PRODUCT_CODE);
    codesResponse.setProductCodes(productCodes);
    productCodesResponse.setValue(codesResponse);
    masterProductResponse = new GdnRestListResponse<>();
    MasterProductResponse masterProduct = new MasterProductResponse();
    masterProduct.setProductCode(PRODUCT_CODE);
    List<MasterProductResponse> masterProductResponses = new ArrayList<>();
    masterProductResponses.add(masterProduct);
    masterProductResponse.setContent(masterProductResponses);
    masterProductResponse1 = new GdnRestListResponse<>();
    List<MasterProductResponse> masterProductResponses1 = new ArrayList<>();
    masterProductResponses1.add(new MasterProductResponse());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pbpFeign);
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  private MasterProductDownloadRequest generateMasterProductRequest() {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.sortBy(SORTBY).reviewPending(true).filterName(KEYWORD).productCode(PRODUCT_CODE)
        .productName(NAME).categoryName(CATEGORY).categoryCode(CATEGORY)
        .username(GdnMandatoryRequestParameterUtil.getUsername())
        .request(GdnMandatoryRequestParameterUtil.getRequestId()).build();
    return masterProductDownloadBuilder.build();
  }

  private MasterSelectedProductDownloadRequest generateMasterSelectedProductDownloadRequest() {
    MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder builder =
        new MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder();
    builder.productSize(PRODUCT_SIZE).productCodes(productCodes)
        .username(GdnMandatoryRequestParameterUtil.getUsername())
        .request(GdnMandatoryRequestParameterUtil.getRequestId()).build();
    return builder.build();
  }

  @Test
  public void getActiveProductDetailsTestForEmptyContent() throws Exception {
    Mockito.when(
            pbpFeign.filterProductCollectionSummaryByKeywordforBulkDownload(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.eq(CATEGORY),
                Mockito.eq(KEYWORD), Mockito.eq(true), Mockito.eq(SORTBY), Mockito.eq(true), Mockito.eq(true)))
        .thenReturn(productCodesResponse);
    Mockito.when(this.pcbFeign.getProductDetailListByProductCodesforBulkDownload(
            Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.eq(productCodes)))
        .thenReturn(new GdnRestListResponse<>());
    activeProductRepository.getActiveProductDetails(generateMasterProductRequest());
    Mockito.verify(this.pbpFeign)
        .filterProductCollectionSummaryByKeywordforBulkDownload(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.eq(CATEGORY),
            Mockito.eq(KEYWORD), Mockito.eq(true), Mockito.eq(SORTBY), Mockito.eq(true), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .getProductDetailListByProductCodesforBulkDownload(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());

  }

  @Test
  public void getActiveProductDetailsForSelectedDownloadTest() throws Exception {
   activeProductRepository.getActiveProductDetailsForSelectedDownload(generateMasterSelectedProductDownloadRequest());
    Mockito.verify(this.pcbFeign)
        .getProductDetailListByProductCodesforBulkDownload(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());
  }

}
