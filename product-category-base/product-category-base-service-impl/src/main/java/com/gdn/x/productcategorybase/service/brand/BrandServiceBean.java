package com.gdn.x.productcategorybase.service.brand;

import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.service.impl.CacheServiceHelperBean;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrQuery.ORDER;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.SolrFieldNames;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.dto.BrandDTO;
import com.gdn.x.productcategorybase.dto.BrandSummaryFilterDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandWipRepository;
import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.BrandChangeService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.productcategorybase.Constants.COMMA;

@Service
@Transactional(readOnly = true)
public class BrandServiceBean implements BrandService {

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private SequenceRepository sequenceRepository;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private PredefinedAllowedAttributeValueRepository predefinedAllowedAttributeValueRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  @Lazy
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private BrandChangeService brandChangeServiceBean;

  @Autowired
  private BrandWipHistoryService brandWipHistoryService;

  @Autowired
  @Lazy
  private BrandWipService brandWipService;

  @Autowired
  private BrandWipRepository brandWipRepository;

  @Autowired
  @Lazy
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  @Qualifier("brandCollectionClient")
  private CloudSolrClient solrClientBrand;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private CacheServiceHelperBean cacheServiceHelperBean;

  private static final Logger LOGGER = LoggerFactory.getLogger(BrandServiceBean.class);
  private static final String ID = "id";
  private static final String BRAND_CODE = "brand_code";
  private static final String BRAND_VALUE = "brand_value";
  private static final String SOLR_QUERY = "q";
  private static final String SOLR_QUERY_NAME_PARAM = "brand_value:";
  private static final String SUFFIX = "*";
  private static final String ESCAPE_SPACE = " ";
  private static final String ESCAPE_VALUE = "\\\\ ";
  private static final String APPROVED_STATUS = "APPROVED";
  private static final String DRAFT_STATUS = "DRAFT";
  private static final String UPDATED_DATE = "updatedDate";
  private static final String ATTRIBUTE_CODE_FOR_BRAND = "BR-M036969";
  private static final String BRAND_NAME = "brandName";
  private static final String OLD_VALUE = "oldValue : ";
  private static final String NEW_VALUE = "newValue : ";

  @Value("${no.brand.switch}")
  private boolean noBrandSwitch;
  @Autowired
  private BrandAuthorisationServiceBean brandAuthorisationServiceBean;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String create(Brand brand) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    Brand savedBrand =
        this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brand.getBrandName(), false);
    if (savedBrand != null) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS,
          "Brand with name " + brand.getBrandName() + " already exist");
    }
    brand.setStoreId(storeId);
    brand.setBrandCode(this.generateBrandCode());
    this.brandRepository.save(brand);
    this.createPredefinedAllowedAttributeValueBrand(storeId, brand);
    brandChangeServiceBean.createSolrDocumentForBrandCollection(brand);
    domainEventPublisherService.publishBrandUpdated(brand);
    return brand.getBrandCode();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public UpdateBrandlogoPath update(Brand brand, String brandRequestCode, String brandLogo, String profileBanner,
      String username, Boolean skuCreationAllowedForAllSellers)
      throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    Brand savedBrand =
        this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brand.getBrandCode());
    Optional.ofNullable(savedBrand).orElseThrow(() -> new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
        "Brand with brand code " + brand.getBrandCode() + " is not found"));
    String oldDescription = Optional.ofNullable(savedBrand.getBrandDescription()).map(String::new).orElse(null);
    boolean oldValidBrandFlag = savedBrand.isValidBrand();
    boolean oldProtectedBrandFlag = savedBrand.isProtectedBrand();
    boolean oldSkuCreationAllowedForAllSellersFlag = savedBrand.isSkuCreationAllowedForAllSellers();
    boolean clearBrandAuthorizationCache = savedBrand.isProtectedBrand() != brand.isProtectedBrand();
    UpdateBrandlogoPath updateBrandlogoPath = new UpdateBrandlogoPath();
    String brandLogoPath = savedBrand.getBrandLogoPath();
    updateBrandlogoPath.setBrandLogoPath(brandLogoPath);
    savedBrand.setBrandDescription(brand.getBrandDescription());
    savedBrand.setBrandLogoPath(brand.getBrandLogoPath());
    savedBrand.setProfileBannerPath(brand.getProfileBannerPath());
    savedBrand.setValidBrand(brand.isValidBrand());
    savedBrand.setProtectedBrand(brand.isProtectedBrand());
    if(Objects.nonNull(skuCreationAllowedForAllSellers)) {
      savedBrand.setSkuCreationAllowedForAllSellers(brand.isSkuCreationAllowedForAllSellers());
    }
    this.brandRepository.save(savedBrand);
    if (oldValidBrandFlag != brand.isValidBrand()) {
      brandWipService.updateValidBrandFlag(username, brand.getBrandCode(), brand.isValidBrand());
    }
    BrandWip brandWip = generateBrandWip(brandRequestCode, brand.getBrandCode(), BrandWipState.UPDATED);
    String historyChanges =
        generateBrandDiff(oldDescription, brand, brandLogo, profileBanner,
            oldValidBrandFlag, oldProtectedBrandFlag, oldSkuCreationAllowedForAllSellersFlag);
    if (StringUtils.isNotEmpty(historyChanges)) {
      brandWipHistoryService
          .generateBrandWipHistory(brandWip, historyChanges, GdnMandatoryParameterUtil.getUsername());
    }
    if (clearBrandAuthorizationCache) {
      brandChangeServiceBean.updateProtectedBrand(savedBrand.getBrandWipId(), savedBrand.isProtectedBrand());
      applicationCacheServiceBean.evictBrandAuthorizationCache(brand.getBrandCode());
      applicationCacheServiceBean.evictProtectedBrandCache(storeId);
    }
    this.domainEventPublisherService.publishBrandUpdated(savedBrand);
    return updateBrandlogoPath;
  }

  private BrandWip generateBrandWip(String brandRequestCode, String brandCode, BrandWipState state) {
    BrandWip brandWip = new BrandWip();
    brandWip.setBrandRequestCode(brandRequestCode);
    brandWip.setBrandCode(brandCode);
    brandWip.setState(state);
    return brandWip;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String delete(String brandCode, String brandDeletedReason) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    String username = GdnMandatoryParameterUtil.getUsername();
    Brand savedBrand = this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
    if (savedBrand == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand with brand code " + brandCode + " is not found");
    }
    Long productCount = productRepository.countByStoreIdAndBrandIgnoreCase(storeId, savedBrand.getBrandName());
    if (productCount > 0) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "Brand with brand code " + brandCode + " is being used by some product");
    }
    this.brandRepository.deleteByStoreIdAndBrandCode(storeId, username, brandCode);
    if (savedBrand.isProtectedBrand()) {
      applicationCacheServiceBean.evictBrandAuthorizationCache(brandCode);
      applicationCacheServiceBean.evictProtectedBrandCache(storeId);
    }
    this.deletePredefinedAllowedAttributeValueBrand(storeId, brandCode);
    BrandWip brandWip = brandWipService.getBrandWipByStoreIdAndBrandCode(storeId, brandCode);
    if (Objects.nonNull(brandWip)) {
      brandWipService.deleteBrandWip(brandWip);
      brandWipHistoryService.generateBrandWipHistory(brandWip,
          brandWip.getState().getDescription() + Constants.HYPHEN + brandDeletedReason,
          GdnMandatoryParameterUtil.getUsername());
    }
    domainEventPublisherService.publishSolrDeleteBrandEvent(
        SolrDeleteBrandDomainEventModel.builder().ids(Collections.singletonList(savedBrand.getBrandWipId())).build());
    domainEventPublisherService.publishBrandDeleted(storeId, brandCode);
    return savedBrand.getBrandName();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Brand undelete(Brand brand) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    String username = GdnMandatoryParameterUtil.getUsername();
    Brand savedBrand =
        this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brand.getBrandName(), true);
    if (savedBrand == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand with brand name " + brand.getBrandName() + " is not found");
    }
    savedBrand.setBrandName(brand.getBrandName());
    savedBrand.setBrandDescription(brand.getBrandDescription());
    savedBrand.setBrandLogoPath(brand.getBrandLogoPath());
    savedBrand.setProfileBannerPath(brand.getProfileBannerPath());
    savedBrand.setMarkForDelete(false);
    savedBrand.setCreatedBy(username);
    savedBrand.setCreatedDate(Calendar.getInstance().getTime());
    savedBrand.setValidBrand(brand.isValidBrand());
    savedBrand.setBrandWipId(brand.getBrandWipId());
    savedBrand.setProtectedBrand(brand.isProtectedBrand());
    this.brandRepository.save(savedBrand);
    this.undeletePredefinedAllowedAttributeValueBrand(storeId, savedBrand.getBrandCode());
    return savedBrand;
  }

  @Override
  public Brand findByBrandCode(String brandCode) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    return this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
  }

  @Override
  public Brand findByBrandName(String brandName, boolean markForDelete) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    return this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brandName, markForDelete);
  }

  @Override
  public Page<Brand> findSummaryByFilter(BrandSummaryFilterDTO filter, String storeId) throws Exception {
    List<Brand> result = new ArrayList<>();
    QueryResponse queryResponse;
    long totalRecords = 0;
    SolrQuery query = null;
    String brandName = StringUtils.isNotEmpty(filter.getBrandName()) ?
        filter.getBrandName().replaceAll(ESCAPE_SPACE, ESCAPE_VALUE) :
        StringUtils.EMPTY;
    if (filter.isMarkForDelete()) {
      Page<Brand> brandPage =
          this.brandRepository.findByStoreIdAndBrandNameAndMarkForDelete(storeId, brandName, true, filter.getPageable());
      result = brandPage.getContent();
      totalRecords = brandPage.getTotalElements();
    } else {
      try {
        // Escaping special chars in buildSolrQueryForBrandSummary
        query = buildSolrQueryForBrandSummary(filter);
        queryResponse = solrClientBrand.query(query);
        if (Objects.nonNull(queryResponse)) {
          SolrDocumentList solrDocumentList = queryResponse.getResults();
          if (Objects.nonNull(solrDocumentList)) {
            totalRecords = solrDocumentList.getNumFound();
            List<String> brandIds =
                solrDocumentList.stream().map(solrDocument -> solrDocument.getFieldValue(ID)).map(String::valueOf)
                    .collect(Collectors.toList());
            result = brandRepository.findByBrandWipIdIn(brandIds);
          }
        }
      } catch (SolrServerException | IOException e) {
        LOGGER.error("SolrServerException caught while executing {} query", query, e);
      }
    }
    Optional<Comparator<Brand>> comparator =
        Optional.of(filter).filter(this::isSortByUpdatedDate).map(BrandSummaryFilterDTO::getSortDirection)
            .map(sortDirection -> {
              if (ORDER.desc.name().equalsIgnoreCase(sortDirection)) {
                return Comparator.comparing(Brand::getUpdatedDate).reversed();
              } else {
                return Comparator.comparing(Brand::getUpdatedDate);
              }
            });
    if (comparator.isPresent()) {
      result.sort(comparator.get());
    }
    return new PageImpl<>(result, filter.getPageable(), totalRecords);
  }

  private SolrQuery buildSolrQueryForBrandSummary(BrandSummaryFilterDTO filter) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder queryBuilder =
        new StringBuilder(SolrFieldNames.BRAND_APPROVED)
            .append(SolrConstants.COLON)
            .append(true);
    if (StringUtils.isNotBlank(filter.getBrandName())) {
      queryBuilder.append(StringUtils.SPACE)
          .append(SolrConstants.AND)
          .append(StringUtils.SPACE)
          .append(SolrFieldNames.BRAND_VALUE)
          .append(SolrConstants.COLON)
          .append(ClientUtils.escapeQueryChars(filter.getBrandName()))
          .append(SUFFIX);
    }
    solrQuery.setQuery(queryBuilder.toString());
    if (Objects.nonNull(filter.getUpdatedDate())) {
      StringBuilder filterQuery = new StringBuilder();
      filterQuery.append(SolrFieldNames.UPDATED_DATE)
          .append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_SQUARE_BRACKET)
          .append(DateTimeFormatter.ISO_INSTANT.format(filter.getUpdatedDate().toInstant()))
          .append(StringUtils.SPACE)
          .append(SolrConstants.TO)
          .append(StringUtils.SPACE)
          .append(SUFFIX)
          .append(SolrConstants.CLOSING_SQUARE_BRACKET);
      solrQuery.addFilterQuery(filterQuery.toString());
    }
    if (isSortByUpdatedDate(filter)) {
      solrQuery.setSort(SolrFieldNames.UPDATED_DATE, ORDER.valueOf(filter.getSortDirection()));
    }
    solrQuery.setRows(filter.getPageable().getPageSize());
    solrQuery.setStart(filter.getPageable().getPageNumber() * filter.getPageable().getPageSize());
    return solrQuery;
  }

  private boolean isSortByUpdatedDate(BrandSummaryFilterDTO filter) {
    return UPDATED_DATE.equalsIgnoreCase(filter.getSortedBy()) &&
        (ORDER.desc.name().equals(filter.getSortDirection()) || ORDER.asc.name()
            .equals(filter.getSortDirection()));
  }

  @Override
  public Page<Brand> findSummaryByName(String brandName, Pageable pageable) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    return this.brandRepository.findByStoreIdAndBrandNameAndMarkForDeleteFalseOrderByBrandName(storeId, brandName, pageable);
  }

  @Override
  public List<BrandDTO> findBrandNamesByBrandCodes(List<String> brandCodes) throws Exception {
    List<BrandDTO> brandDTOList = new ArrayList<>();
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    List<Brand> brandList = this.brandRepository.findByStoreIdAndBrandCodesAndMarkForDeleteFalse(storeId, brandCodes);
    for (Brand brand : brandList) {
      if (StringUtils.isNotEmpty(brand.getBrandName())) {
        BrandDTO brandDTO = new BrandDTO();
        brandDTO.setBrandCode(brand.getBrandCode());
        brandDTO.setBrandName(brand.getBrandName());
        brandDTOList.add(brandDTO);
      }
    }
    return brandDTOList;
  }

  @Override
  public Page<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String storeId, String value,
      String businessPartnerCode, Pageable pageable, boolean isSearch, boolean isExternal) throws Exception {
    long totalRecords = 0;
    value = StringUtils.isNotEmpty(value) ? ClientUtils.escapeQueryChars(value) : StringUtils.EMPTY;
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueList = new ArrayList<>();
    SolrQuery query = null;
    query = buildSolrQueryForBrandSuggestions(value, businessPartnerCode, pageable, isExternal, isSearch);
    QueryResponse queryResponse = solrClientBrand.query(query);
    if (Objects.nonNull(queryResponse)) {
      SolrDocumentList solrDocumentList = queryResponse.getResults();
      if (Objects.nonNull(solrDocumentList)) {
        totalRecords = solrDocumentList.getNumFound();
        predefinedAllowedAttributeValueList =
            solrDocumentList.stream().map(this::getPredefinedAttributeValueResposnse).collect(Collectors.toList());
      }
    }
    return new PageImpl<>(predefinedAllowedAttributeValueList, pageable, totalRecords);
  }

  @Override
  public BrandPredefinedAttributeValueResponse getBrandPredefinedValueByCodeAndState(String storeId, String brandCode,
      String status) throws Exception {
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse =
        new BrandPredefinedAttributeValueResponse();
    if (Constants.DRAFT_STATUS.equalsIgnoreCase(status)) {
      BrandWipResponse brandWipResponse =
          brandWipService.filterByBrandRequestCodeIrrespectiveOfState(storeId, brandCode);
      if (Constants.REJECTED_STATUS.equalsIgnoreCase(brandWipResponse.getState()) || Constants.DELETED_STATUS
          .equalsIgnoreCase(brandWipResponse.getState())) {
        return null;
      } else if (Constants.APPROVED_STATUS.equalsIgnoreCase(brandWipResponse.getState())) {
        brandCode = brandWipResponse.getBrandCode();
      }
      brandPredefinedAttributeValueResponse.setBrandRequestCode(brandWipResponse.getBrandRequestCode());
      brandPredefinedAttributeValueResponse.setBrandApprovalStatus(brandWipResponse.getState());
    } else {
      brandPredefinedAttributeValueResponse.setBrandApprovalStatus(Constants.APPROVED_STATUS);
    }
    PredefinedAllowedAttributeValue predefinedAttributeValuesResponse =
        this.predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCode(storeId, brandCode);
    if (Objects.nonNull(predefinedAttributeValuesResponse)) {
      BeanUtils.copyProperties(predefinedAttributeValuesResponse, brandPredefinedAttributeValueResponse,
          "brandApprovalStatus", "brandRequestCode");
    } else {
      return null;
    }
    return brandPredefinedAttributeValueResponse;
  }

  private SolrQuery buildSolrQueryForBrandSuggestions(String value, String businessPartnerCode, Pageable pageable,
      boolean isExternal, boolean isSearch) {
    SolrQuery solrQuery = new SolrQuery();
    if (StringUtils.isBlank(value)) {
      solrQuery.setQuery(SolrConstants.DEFAULT_QUERY);
      if (isExternal && isSearch) {
        solrQuery.addFilterQuery(
            new StringBuilder(SolrFieldNames.BRAND_APPROVED).append(SolrConstants.COLON).append(Boolean.FALSE)
                .toString());
        solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
            .append(businessPartnerCode).toString());
      }
    } else {
      StringBuilder builder = new StringBuilder();
      builder.append(SolrFieldNames.BRAND_VALUE).append(SolrConstants.COLON).append(value).append(SUFFIX);
      solrQuery.setQuery(builder.toString());
      if (isExternal) {
        setFilterQuery(businessPartnerCode, solrQuery);
      } else {
        StringBuilder filterQuery = new StringBuilder();
        filterQuery.append(SolrFieldNames.BRAND_APPROVED).append(SolrConstants.COLON).append(Boolean.TRUE);
        solrQuery.addFilterQuery(filterQuery.toString());
      }
    }
    solrQuery.setSort(SolrFieldNames.BRAND_VALUE, ORDER.asc);
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    return solrQuery;
  }

  private void setFilterQuery(String businessPartnerCode, SolrQuery solrQuery) {
    StringBuilder filterQuery = new StringBuilder();
    filterQuery.append(SolrConstants.OPEN_BRACKET).append(SolrConstants.OPEN_BRACKET)
        .append(SolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON).append(businessPartnerCode)
        .append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE)
        .append(SolrFieldNames.BRAND_APPROVED).append(SolrConstants.COLON).append(Boolean.FALSE)
        .append(SolrConstants.CLOSING_BRACKET).append(StringUtils.SPACE).append(SolrConstants.OR)
        .append(StringUtils.SPACE).append(SolrFieldNames.BRAND_APPROVED).append(SolrConstants.COLON)
        .append(Boolean.TRUE).append(SolrConstants.CLOSING_BRACKET);
    solrQuery.addFilterQuery(filterQuery.toString());
  }

  private List<PredefinedAllowedAttributeValueResponse> toPredefinedAllowedAttributeValueResponseList(
      List<Object[]> brandWips) {
    return brandWips.stream().map(this::toPredefinedAllowedAttributeValueResponse).collect(Collectors.toList());
  }

  private PredefinedAllowedAttributeValueResponse toPredefinedAllowedAttributeValueResponse(Object[] brandWip) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(String.valueOf(brandWip[0]));
    predefinedAllowedAttributeValueResponse.setValue(String.valueOf(brandWip[1]));
    boolean brandApproved = BrandWipState.APPROVED.equals(String.valueOf(brandWip[2])) ? true : false;
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus(getBrandApprovalStatus(brandApproved));
    return predefinedAllowedAttributeValueResponse;
  }

  private PredefinedAllowedAttributeValueResponse generatePredefinedAllowedAttributeValue(Brand brand) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValue.setId(brand.getId());
    predefinedAllowedAttributeValue.setStoreId(brand.getStoreId());
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(brand.getBrandCode());
    predefinedAllowedAttributeValue.setValue(brand.getBrandName());
    predefinedAllowedAttributeValue.setBrandApprovalStatus(APPROVED_STATUS);
    return predefinedAllowedAttributeValue;
  }

  private PredefinedAllowedAttributeValueResponse generatePredefinedAllowedAttributeValueFromBrandwip(BrandWip brand) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValue.setId(brand.getId());
    predefinedAllowedAttributeValue.setStoreId(brand.getStoreId());
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(brand.getBrandRequestCode());
    predefinedAllowedAttributeValue.setValue(brand.getBrandName());
    predefinedAllowedAttributeValue.setBrandApprovalStatus(brand.getState().name());
    return predefinedAllowedAttributeValue;
  }

  /**
   * generate PredefinedAllowedAttributeValue from Solr document
   *
   * @param solrDocument
   * @return
   */
  private PredefinedAllowedAttributeValueResponse getPredefinedAttributeValueResposnse(SolrDocument solrDocument) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValueResponse();
    String brandApprovalStatus;
    predefinedAllowedAttributeValue
        .setPredefinedAllowedAttributeCode(String.valueOf(solrDocument.getFieldValue(BRAND_CODE)));
    predefinedAllowedAttributeValue.setValue(String.valueOf(solrDocument.getFieldValue(BRAND_VALUE)));
    predefinedAllowedAttributeValue.setStoreId(GdnMandatoryParameterUtil.getStoreId());
    predefinedAllowedAttributeValue.setId(String.valueOf(solrDocument.getFieldValue(ID)));
    brandApprovalStatus = getBrandApprovalStatus(
        Boolean.valueOf(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BRAND_APPROVED))));
    predefinedAllowedAttributeValue.setBrandApprovalStatus(brandApprovalStatus);
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PROTECTED_BRAND))) {
      predefinedAllowedAttributeValue.setProtectedBrand(
          (boolean) solrDocument.getFieldValue(SolrFieldNames.PROTECTED_BRAND));
    }
    return predefinedAllowedAttributeValue;
  }

  private String getBrandApprovalStatus(boolean status) {
    String brandApprovalStatus;
    if (status) {
      brandApprovalStatus = APPROVED_STATUS;
    } else {
      brandApprovalStatus = DRAFT_STATUS;
    }
    return brandApprovalStatus;
  }


  private String generateBrandCode() throws Exception {
    return BrandService.PREFIX_BRAND_CODE + "-" + StringUtils
        .leftPad(String.valueOf(this.sequenceRepository.findByCode(BrandService.PREFIX_BRAND_CODE)), 5, '0');
  }

  private void createPredefinedAllowedAttributeValueBrand(String storeId, Brand brand) throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    List<Attribute> attributes =
        this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(storeId, "Brand", pageable)
            .getContent();
    if (!attributes.isEmpty()) {
      Attribute attribute = this.attributeRepository.findById(attributes.get(0).getId()).orElse(null);
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
      predefinedAllowedAttributeValue.setStoreId(storeId);
      predefinedAllowedAttributeValue.setAttribute(attribute);
      predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(brand.getBrandCode());
      predefinedAllowedAttributeValue.setValue(brand.getBrandName());
      predefinedAllowedAttributeValue.setSequence(0);
      this.predefinedAllowedAttributeValueRepository.save(predefinedAllowedAttributeValue);
    }
  }

  private void deletePredefinedAllowedAttributeValueBrand(String storeId, String brandCode) throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    List<Attribute> attributes =
        this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(storeId, "Brand", pageable)
            .getContent();
    if (!attributes.isEmpty()) {
      this.predefinedAllowedAttributeValueRepository
          .deleteByStoreIdAndAttributeIdAndCode(storeId, attributes.get(0).getId(), brandCode);
      this.attributeService
          .evictAttributeCache(storeId, attributes.get(0).getId(), attributes.get(0).getAttributeCode());
    }
  }

  private void undeletePredefinedAllowedAttributeValueBrand(String storeId, String brandCode) throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    List<Attribute> attributes =
        this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(storeId, "Brand", pageable)
            .getContent();
    if (!attributes.isEmpty()) {
      this.predefinedAllowedAttributeValueRepository
          .undeleteByStoreIdAndAttributeIdAndCode(storeId, attributes.get(0).getId(), brandCode);
    }
  }

  public String generateBrandDiff(String oldBrandDescription, Brand newBrand, String brandLogo,
      String profileBanner, boolean oldValidBrandFlag, boolean oldProtectedBrandFlag,
      boolean oldSkuCreationAllowedForAllSellersFlag) throws JsonProcessingException {
    String newBrandDescription = new String(newBrand.getBrandDescription());
    Map<String, String> brandDiffs = new HashMap<>();
    if (Objects.isNull(oldBrandDescription) && StringUtils.isNotEmpty(newBrandDescription)) {
      brandDiffs.put("brandDescription", "o: , n: " + newBrandDescription);
    } else {
      if (!StringUtils.equals(oldBrandDescription, newBrandDescription)) {
        brandDiffs.put("brandDescription", "o: " + oldBrandDescription + ", n: " + newBrandDescription);
      }
    }
    if (StringUtils.isNotEmpty(brandLogo)) {
      brandDiffs.put("brandLogo", "n: " + brandLogo);
    }
    if (StringUtils.isNotEmpty(profileBanner)) {
      brandDiffs.put("profileBanner", "n: " + profileBanner);
    }
    if (oldValidBrandFlag != newBrand.isValidBrand()) {
      brandDiffs.put("showBrand", "o: " + oldValidBrandFlag + ", n: " + newBrand.isValidBrand());
    }
    if (oldProtectedBrandFlag != newBrand.isProtectedBrand()) {
      brandDiffs.put("Protected Brand", new StringBuilder().append("o: ").append(oldProtectedBrandFlag).append(", n: ")
          .append(newBrand.isProtectedBrand()).toString());
    }
    if (oldSkuCreationAllowedForAllSellersFlag != newBrand.isSkuCreationAllowedForAllSellers()) {
      brandDiffs.put("Sku creation allowed for all sellers", new StringBuilder().append("o: ")
          .append(oldSkuCreationAllowedForAllSellersFlag).append(", n: ")
          .append(newBrand.isSkuCreationAllowedForAllSellers()).toString());
    }
    return this.objectMapper.writeValueAsString(brandDiffs);
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getDefaultBrands(String storeId) {
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueList = new ArrayList<>();
    List<Brand> brandList;
    if (noBrandSwitch) {
      brandList = this.brandRepository.findNoBrandByStoreId(storeId);
    } else {
      brandList = this.brandRepository.findByStoreIdAndDefaultBrands(storeId);
    }
    if (CollectionUtils.isNotEmpty(brandList)) {
      predefinedAllowedAttributeValueList =
          brandList.stream().map(this::generatePredefinedAllowedAttributeValue).collect(Collectors.toList());
    }
    return predefinedAllowedAttributeValueList;
  }

  @Override
  public Page<BrandSummaryResponse> getBrandSummaryResponseForValidBrands(String storeId, int page, int size) {
    long maxAllowedSize = Long.parseLong(
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY)
            .getValue());
    if (maxAllowedSize < size) {
      LOGGER.error("Size is greater than maximum allowed size. [size = {}, maxAllowedSize = {}]", size, maxAllowedSize);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(ErrorMessage.GREATER_THAN_MAX_ALLOWED_SIZE_ERROR.getMessage(), size, maxAllowedSize));
    }
    Page<Brand> brands =
        brandRepository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(storeId, PageRequest.of(page, size));
    List<BrandSummaryResponse> brandSummaryResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(brands.getContent())) {
     brandSummaryResponses  = ConverterUtil.convertToBrandSummaryResponse(brands.getContent());
    }
    return new PageImpl<>(brandSummaryResponses, PageRequest.of(page, size), brands.getTotalElements());
  }

  @Override
  @Cacheable(value = CacheNames.BRAND, key = "#storeId+'_'+#brandCode", unless = "#result == null")
  public BrandResponse findByBrandCodeCached(String storeId, String brandCode) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode), String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    return ConverterUtil.generateBrandResponse(
        this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode));
  }

  @Override
  @Cacheable(value = CacheNames.PROTECTED_BRANDS, key = "#storeId", unless = "#result == null")
  public List<ProtectedBrandResponse> getProtectedBrandList(String storeId) {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    List<ProtectedBrandResponse> protectedBrandListResponse = new ArrayList<>();
    List<Brand> protectedBrandsList =
      brandRepository.findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(storeId);
    if (CollectionUtils.isNotEmpty(protectedBrandsList)) {
      protectedBrandsList.forEach(brand -> {
        protectedBrandListResponse.add(ProtectedBrandResponse.builder().brandCode(brand.getBrandCode())
          .brandName(brand.getBrandName()).build());
      });
    }
    return protectedBrandListResponse;
  }

  @Override
  public BrandResponse getBrandResponseByBrandName(String storeId, String brandName,
    boolean markForDelete, boolean activeBrandsOnly) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandName),
      String.valueOf(ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK));
    String filteredBrandNameForCache  =
      brandName.replace(Constants.SPACE, Constants.HYPHEN).toLowerCase();
    return cacheServiceHelperBean.findByBrandName(storeId, brandName, markForDelete,
      activeBrandsOnly, filteredBrandNameForCache);
  }

  @Override
  public BrandResponse getBrandResponseByBrandCode(String storeId, String brandCode)
    throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode),
      String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    return cacheServiceHelperBean.findByBrandCode(storeId, brandCode);
  }

  @Override
  public void deleteAllBrandCache(String storeId, String brandName, String brandCode) {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode),
      String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandName),
      String.valueOf(ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK));
    cacheServiceHelperBean.evictBrandCacheByCode(storeId, brandCode);
    cacheServiceHelperBean.evictBrandCacheByNameAndActiveBrandsOnly(storeId, brandName);
    cacheServiceHelperBean.evictBrandCacheByNameAndMarkForDelete(storeId, brandName);
  }

  @Override
  @Transactional(readOnly = true)
  public String getBrandCodeByProductCode(String storeId, String productCode) {
    Product product =
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    Hibernate.initialize(product.getProductAttributes());
    for (ProductAttribute productAttribute : Optional.ofNullable(product.getProductAttributes())
        .orElse(Collections.emptyList())) {
      if (!productAttribute.isMarkForDelete()) {
        Hibernate.initialize(productAttribute.getAttribute());
        if (ATTRIBUTE_CODE_FOR_BRAND.equals(productAttribute.getAttribute().getAttributeCode())) {
          Hibernate.initialize(productAttribute.getProductAttributeValues());
          for (ProductAttributeValue productAttributeValue : Optional.ofNullable(
              productAttribute.getProductAttributeValues()).orElse(Collections.emptyList())) {
            if (!productAttributeValue.isMarkForDelete()) {
              return productAttributeValue.getPredefinedAllowedAttributeValue()
                  .getPredefinedAllowedAttributeCode();
            }
          }
        }
      }
    }
    return StringUtils.EMPTY;
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Triple<BrandHistoryEventModel, BrandWip, Brand> updateOnlyBrandName(String storeId,
      String brandName, String brandCode)
      throws Exception {
    Brand brand =
        brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
    checkArgument(Objects.nonNull(brand),
        StringUtils.join(ErrorMessage.BRAND_NOT_FOUND.getMessage(), brandCode));
    String oldBrandName = brand.getBrandName();
    BrandWip brandWip = brandWipService.updateBrandName(storeId, brandCode, brandName);
    brandAuthorisationServiceBean.updateBrandNameByBrandCode(oldBrandName, brandName, brandCode);
    brand.setBrandName(brandName);
    Map<String, String> description = new HashMap<>();
    description.put(BRAND_NAME,
        new StringBuilder(OLD_VALUE).append(oldBrandName).append(COMMA).append(NEW_VALUE)
            .append(brandName).toString());
    String descriptionString = objectMapper.writeValueAsString(description);
    BrandHistoryEventModel brandHistoryEventModel = ConverterUtil.toBrandWipHistoryModel(brandWip, descriptionString,
        GdnMandatoryParameterUtil.getUsername());
    predefinedAllowedAttributeValueService.updatePredefinedAllowedAttributeCodeForApprovedBrand(
        storeId, brandCode, brand);
    Brand updatedBrand = brandRepository.save(brand);
    deleteAllBrandCache(storeId, oldBrandName, brandCode);
    String filteredBrandNameForCache =
        brandName.replace(Constants.SPACE, Constants.HYPHEN).toLowerCase();
    cacheServiceHelperBean.evictBrandCacheByNameAndActiveBrandsOnly(storeId,
        filteredBrandNameForCache);
    return Triple.of(brandHistoryEventModel, brandWip, updatedBrand);
  }

}
