package com.gdn.partners.pdt.service.distribution;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.feign.PCBFeign;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.repository.sequence.SequenceRepository;
import com.gdn.partners.pdt.service.product.ProductService;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

@Service
@Transactional(readOnly = true)
@Slf4j
public class DistributionTaskServiceBean implements DistributionTaskService {

  @Value("${allow.replace.product.data.states}")
  private String allowReplaceProductData ;

  private static final String INTERNAL_BUSINESS_PARTNER = "INTERNAL";

  @Autowired
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Autowired
  private VendorRepository vendorRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductReviewerRepository productReviewerRepository;

  @Autowired
  @Qualifier(value = "neoProductService")
  private ProductService productService;

  @Autowired
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Autowired
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Autowired
  private SequenceRepository sequenceRepository;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Value("${auto.distribute.switch}")
  private Boolean autoDistributeSwitch;

  @Value("${auto.distribute.default.vendorCode}")
  private String autoDistributeDefaultVendorCode;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product autoDistribute(Product product, boolean isForReview) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    Product savedProduct =
        this.productRepository.findByProductCode(product.getProductCode());
    if (Objects.isNull(savedProduct)) {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, product.getUpdatedBy());
      Vendor vendor = null;
      if (!StringUtils.equalsIgnoreCase(product.getBusinessPartnerCode(), INTERNAL_BUSINESS_PARTNER) || isForReview) {
        String vendorCode;
        if (autoDistributeSwitch) {
          vendorCode = this.autoDistributionConfigurationRepository
              .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(storeId,
                  Arrays.asList(product.getCreatedBy(), product.getBusinessPartnerCode(), product.getCategoryCode()));
        } else {
          vendorCode = autoDistributeDefaultVendorCode;
        }
        vendor = this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(vendorCode);
        product.setStoreId(storeId);
        if (vendor != null) {
          product.setCurrentVendor(vendor);
        }
      }
      product.setState(WorkflowState.IN_REVIEW);
      product.setProductCreatedDate(Calendar.getInstance().getTime());
      if (Objects.isNull(product.getReviewType())) {
        product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
      }
      ProductReviewer productReviewer =
          ProductReviewer.builder().approvedDate(null).assignedDate(null)
              .approverAssignee(null).productCode(product.getProductCode()).build();
      productReviewer.setStoreId(product.getStoreId());
      productReviewer.setCreatedBy(product.getCreatedBy());
      log.info("autoDistribute product code : {} : {} : {}", product.getProductCode(), product.getCreatedBy(),
          product);
      this.productRepository.save(product);
      this.productReviewerRepository.save(productReviewer);
      if (Objects.nonNull(vendor)) {
        List<ProductDistributionTask> distributionTasks =
            this.generateDistributionTask(storeId, vendor, List.of(product), WorkflowState.IN_REVIEW);
        this.distributionTaskRepository.updateProductDistributionTask(List.of(product));
        this.distributionTaskRepository.saveAll(distributionTasks);
        this.distributionTaskHistoryService.create(distributionTasks);
      }
      return product;
    } else {
      if (Arrays.stream(allowReplaceProductData.split(Constants.COMMA)).map(WorkflowState::valueOf)
          .collect(Collectors.toSet()).contains(savedProduct.getState())) {
        savedProduct.setMarkForDelete(false);
        this.productService.overwrite(savedProduct, product);
      } else {
        log.error("autoDistribute product is not allowed to replace with Product Code : {}",
            savedProduct.getProductCode());
      }
    }
    return savedProduct;
  }

  @Override
  public void publishVendorApprovedEvent(Product product, boolean reviewPending) throws Exception {
    approvedProductPublisherService.publishVendorApprovedEvent(product, reviewPending);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void assignee(String vendorCode, List<String> productCodes) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    Vendor vendor = this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(vendorCode);
    if (vendor == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "invalid vendor");
    }
    List<Product> products =
        this.productRepository.findByProductCode(productCodes,
            Arrays.asList(WorkflowState.IN_REVIEW, WorkflowState.UNASSIGNED));
    if (CollectionUtils.isEmpty(products)) {
      return;
    } else {
      productCodes = products.stream().map(Product::getProductCode).collect(Collectors.toList());
    }
    for (Product product : products) {
      product.setState(WorkflowState.IN_REVIEW);
      product.setCurrentVendor(vendor);
      product.setProductCreatedDate(Calendar.getInstance().getTime());
    }
    List<ProductDistributionTask> distributionTasks = this.generateDistributionTask(storeId, vendor, products,
        WorkflowState.IN_REVIEW);
    this.distributionTaskRepository.updateProductDistributionTask(products);
    this.distributionTaskRepository.saveAll(distributionTasks);
    this.productRepository.saveAll(products);
    this.distributionTaskHistoryService.create(distributionTasks);
    this.solrVendorCollectionService
        .assignProductToVendorAtomicUpdate(storeId, vendorCode, productCodes);
  }

  @Override
  public List<ProductDistributionTask> generateDistributionTaskForProduct(String storeId, Vendor vendor,
      List<Product> products, WorkflowState workflowState) throws Exception {
    return generateDistributionTask(storeId, vendor, products, workflowState);
  }

  private List<ProductDistributionTask> generateDistributionTask(String storeId, Vendor vendor, List<Product> products,
      WorkflowState workflowState)
      throws Exception {
    Date slaDate = null;
    List<ProductDistributionTask> distributionTasks = new ArrayList<ProductDistributionTask>();
    for (Product product : products) {
      String distributionTaskCode =
          vendor.getVendorCode()
              + "-"
              + StringUtils.leftPad(String.valueOf(this.sequenceRepository.generateByCode(vendor.getVendorCode())), 10,
                  '0');
      ProductDistributionTask distributionTask = new ProductDistributionTask();
      distributionTask.setStoreId(storeId);
      distributionTask.setTaskCode(distributionTaskCode);
      distributionTask.setState(workflowState);
      distributionTask.setRejectedCount(0);
      distributionTask.setVendor(vendor);
      distributionTask.setProduct(product);
      distributionTask.setSlaDate(slaDate);
      distributionTasks.add(distributionTask);
    }
    return distributionTasks;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void clearPresentDistributionTaskAndCreateNewTask(String storeId, Product product) throws Exception {
    Vendor vendor = vendorRepository.findById(product.getVendorId()).get();
    List<ProductDistributionTask> distributionTasks =
        this.generateDistributionTask(storeId, vendor, Collections.singletonList(product), WorkflowState.IN_REVIEW);
    this.distributionTaskRepository.updateProductDistributionTask(Collections.singletonList(product));
    this.distributionTaskRepository.saveAll(distributionTasks);
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCode(String username, String productCode) throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        pcbFeign.getProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
            productCode, true);
    if (!response.isSuccess()) {
      log.error("Error fetching product details for productCode: {} message: {} ", productCode,
          response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ImageQcProcessedAndBrandResponse getImageQcResponseByProductCode(String productCode) {
    return productServiceRepository.getImageQcPredictionResponse(productCode);
  }
}
