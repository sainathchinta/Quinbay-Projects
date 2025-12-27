package com.gdn.partners.pbp.service.productlevel1;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ItemService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.repository.BrandRepository;
import com.gdn.mta.product.repository.BrandWipRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

@Service
@Transactional(readOnly = true)
public class ProductLevel1WipServiceBean implements ProductLevel1WipService {

  private static final Logger LOG = LoggerFactory.getLogger(ProductLevel1WipServiceBean.class);
  private static final String POST_LIVE = "Post-live";
  private static final String BRAND = "Brand";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductLevel1SolrService productLevel1SolrService;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;
  
  @Autowired
  private ProductWorkflowRepository oldProductWorkflowRepository;

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private BrandWipRepository brandWipRepository;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void create(String businessPartnerCode, String businessPartnerName, String productCreationType, ProductRequest request) throws Exception {
    String channelId = mandatoryParameterHelper.getChannelId();
    String clientId = mandatoryParameterHelper.getClientId();
    String requestId = mandatoryParameterHelper.getRequestId();
    String username = mandatoryParameterHelper.getUsername();
    if (StringUtils.isEmpty(businessPartnerCode)) {
      request.setActivated(true);
      request.setViewable(false);
      businessPartnerCode = GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE;
      businessPartnerName = GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_NAME;
    } else {
      request.setActivated(false);
      request.setViewable(false);
    }
    String brandCode;
    request.setReviewPending(true);
    GdnRestSingleResponse<BrandResponse> brandResponse = pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, request.getBrand(), false, true);
    if (Objects.isNull(brandResponse) || Objects.isNull(brandResponse.getValue())) {
      BrandWipResponse brandWipResponse = brandWipRepository.
          findBrandWipByBrandNameAndBusinessPartnerCode(request.getBrand(), businessPartnerCode);
      if (Objects.isNull(brandWipResponse)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Brand is Rejected");
      }
      brandCode = brandWipResponse.getBrandRequestCode();
      request.setBrandApprovalStatus(BrandApprovalStatus.DRAFT.name());
    } else {
      brandCode = brandResponse.getValue().getBrandCode();
      if (CommonUtils.validateProtectedBrand(brandResponse.getValue(), businessPartnerCode)) {
        SimpleBooleanResponse brandAuthResponse = productOutbound
          .authoriseProtectedBrand(request.getStoreId(), channelId, clientId, requestId, username,
            brandCode, businessPartnerCode);
        if (!brandAuthResponse.getResult()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND);
        }
      }
      request.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    }
    request.setCreatedMerchant(businessPartnerCode);
    this.productRepository.create(request);
    List<ConfigurationStatusResponse> responseList = this.productRepository.getConfigurationStatus(ConverterUtil
        .toConfigurationStatusRequestList(businessPartnerCode,
            request.getProductCategories().get(0).getCategory().getCategoryCode()));
    boolean postLive = false;
    if (POST_LIVE.equalsIgnoreCase(responseList.get(0).getReviewConfig())) {
      postLive = true;
    }
    this.productLevel1CollectionService
        .create(businessPartnerCode, businessPartnerName, request.getProductCode(), brandCode,
            request.getBrandApprovalStatus(), postLive, productCreationType );
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductCollection approveDraft(String productCode, ProfileResponse profileResponse) throws Exception {
    this.productRepository.updateActivated(productCode, true);
    ProductCollection productCollection = this.productLevel1CollectionService.approveDraft(productCode, profileResponse);
    itemService.publishItemStatusEvent(productCode, ProductStatus.ACTIVE);
    return productCollection;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveContent(String productCode) throws Exception {
    this.productLevel1CollectionService.update(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateRejectedProduct(String productCode) throws Exception {
    this.productLevel1CollectionService.updateRejectedProduct(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void resubmit(ProductRequest productRequest, Date submitDate) throws Exception {
    this.productOutbound.updateProductContent(productRequest, false);
    this.productRepository.updateProductImage(productRequest);
    this.productLevel1CollectionService.resubmit(productRequest, submitDate);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveImage(String productCode) throws Exception {
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void activate(String productCode) throws Exception {
    this.productLevel1CollectionService.activate(productCode);
    this.productLevel1SolrService.update(productCode);
    this.productRepository.updateViewable(productCode, true);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode, String notes) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.nonNull(productCollection)) {
      ProductDetailResponse product =
          this.productRepository.findDetailById(productCollection.getProductId());
      product.setMarkForDelete(true);
      ProductWorkflow oldProductWorkflow = new ProductWorkflow();
      oldProductWorkflow.setProductId(productCollection.getProductId());
      oldProductWorkflow.setState(ProductWorkflowLookup.STATE_DELETE);
      oldProductWorkflow.setDescription(ProductWorkflowLookup.STATE_DELETE_DESCRIPTION);
      oldProductWorkflow.setNotes(notes);
      oldProductWorkflow.setStoreId(storeId);
      this.oldProductWorkflowRepository.save(oldProductWorkflow);
      if (productCollection.isViewable()) {
        this.productLevel1SolrService.deleteByProductCollectionId(productCollection.getId());
      }
      this.productLevel1CollectionService.delete(productCollection);
      this.productRepository.delete(product);
    } else {
      LOG.warn("product already marked as deleted, productCode : {}", productCode);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void returnDraftForCorrection(String productCode, NeedRevisionNotes needRevisionNotes,
      boolean autoNeedRevision, boolean screeningAction, boolean validateDraftState) throws Exception {
    this.productLevel1CollectionService
        .returnDraftForCorrection(productCode, needRevisionNotes, autoNeedRevision, screeningAction,
            validateDraftState);
  }

}
