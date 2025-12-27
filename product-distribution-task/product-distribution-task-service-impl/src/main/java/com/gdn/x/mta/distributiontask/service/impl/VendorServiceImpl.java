package com.gdn.x.mta.distributiontask.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.VendorFilterRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorQuotaCounterRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.VendorDefaultFilter;
import com.gdn.x.mta.distributiontask.model.VendorQuotaCounter;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.service.impl.util.VendorUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.PivotField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by Alok on 9/16/16.
 */
@Slf4j
@Service
public class VendorServiceImpl implements VendorService {
  public static final String CONTENT_AND_IMAGE_APPROVED = "CONTENT_AND_IMAGE_APPROVED";
  public static final String IN_REVIEW = "IN_REVIEW";
  public static final String CONTENT_APPROVED = "CONTENT_APPROVED";
  public static final String IMAGES_APPROVED = "IMAGES_APPROVED";
  public static final String IMAGE_REJECTED = "IMAGE_REJECTED";
  public static final String CONTENT_REJECTED = "CONTENT_REJECTED";
  @Autowired
  private VendorRepository vendorRepository;
  
  @Autowired
  private VendorUtils vendorUtils;

  @Autowired
  private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Autowired
  @Qualifier(value = "vendorProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Autowired
  private ObjectMapper objectMapper = new ObjectMapper();

  @Autowired
  private VendorFilterRepository vendorFilterRepository;

  public static final String VENDOR_CODE_STATE = "vendorCode,state";

  @Override
  @Transactional
  public Vendor save(Vendor vendor) {
    if (StringUtils.isEmpty(vendor.getId())) {
      createVendorQuotaCounter(vendor);
      return vendorRepository.save(vendor);
    } else {
      Vendor saveVendor = vendorRepository.findById(vendor.getId()).get();
      BeanUtils.copyProperties(vendor, saveVendor, "id");
      return vendorRepository.save(saveVendor);
    }
  }

  private void createVendorQuotaCounter(Vendor vendor) {
    VendorQuotaCounter vendorQuotaCounter = new VendorQuotaCounter();
    vendorQuotaCounter.setVendor(vendor);
    vendorQuotaCounter.setTotalReviewInProgress(0);
    vendorQuotaCounter.setCreatedBy(vendor.getCreatedBy());
    vendorQuotaCounter.setCreatedDate(new Date());
    vendorQuotaCounter.setMarkForDelete(false);
    vendorQuotaCounter.setUpdatedDate(new Date());
    vendorQuotaCounter.setStoreId(vendor.getStoreId());
    vendorQuotaCounterRepository.save(vendorQuotaCounter);
  }

  @Override
  public Page<Vendor> findVendorList(Pageable pageable) {
    return vendorRepository.getVendorList(pageable);
  }

  @Override
  public Vendor findByVendorCode(String vendorCode) {
    return vendorRepository.findByVendorCodeAndMarkForDeleteFalse(vendorCode);
  }
  
  @Override
  public List<VendorCapacityDTO> countVendorsCapacity() {
    List<Object[]> nativeResultSet = this.vendorRepository.countAllVendorRemainingCapacity();
    List<VendorCapacityDTO> vendorCapacityDTOs = new ArrayList<>();
    for(Object[] nativeResult : nativeResultSet){
      vendorCapacityDTOs.add(this.vendorUtils.convertToVendorCapacityDTO(nativeResult));
    }
    return vendorCapacityDTOs;
  }

  @Override
  public List<VendorTaskInformationDTO> countVendorAssignationAndCapacity() throws IOException, SolrServerException {
    List<VendorTaskInformationDTO> vendorTaskInformationDTOs = new ArrayList<>();
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForCountVendorAssignment();
    QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
    List<String> vendorCodes = new ArrayList<>();
    List<Vendor> vendors = vendorRepository.findByMarkForDeleteFalse();
    if (Objects.nonNull(queryResponse)) {
      List<PivotField> pivotFields = queryResponse.getFacetPivot().get(VENDOR_CODE_STATE);
      Map<String, Vendor> vendorMap =
          vendors.stream().collect(Collectors.toMap(Vendor::getVendorCode, Function.identity()));
      if (CollectionUtils.isNotEmpty(pivotFields)) {
        for (PivotField pivotField : pivotFields) {
          if (vendorMap.containsKey(pivotField.getValue())) {
            vendorCodes.add((String) pivotField.getValue());
            VendorTaskInformationDTO vendorTaskInformationDTO = new VendorTaskInformationDTO();
            Map<Object, Integer> stateCountMap =
                pivotField.getPivot().stream().collect(Collectors.toMap(PivotField::getValue, PivotField::getCount));
            setQCCounts(vendorTaskInformationDTO, stateCountMap, vendorMap.get(pivotField.getValue()));
            vendorTaskInformationDTOs.add(vendorTaskInformationDTO);
          }
        }
      }
    }
    List<VendorTaskInformationDTO> newVendors =
        vendors.stream().filter(vendor -> !vendorCodes.contains(vendor.getVendorCode()))
            .map(this::toVendorTaskInformationDTO).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(newVendors)) {
      vendorTaskInformationDTOs.addAll(newVendors);
    }
    return vendorTaskInformationDTOs;
  }

  private VendorTaskInformationDTO toVendorTaskInformationDTO(Vendor vendor) {
    VendorTaskInformationDTO vendorTaskInformationDTO = new VendorTaskInformationDTO();
    vendorTaskInformationDTO.setId(vendor.getId());
    vendorTaskInformationDTO.setName(vendor.getName());
    vendorTaskInformationDTO.setVendorCode(vendor.getVendorCode());
    vendorTaskInformationDTO.setCapacity(vendor.getQuota());
    vendorTaskInformationDTO.setAssignedCount(0);
    vendorTaskInformationDTO.setQcCount(0);
    return vendorTaskInformationDTO;
  }

  private void setQCCounts(VendorTaskInformationDTO vendorTaskInformationDTO, Map<Object, Integer> stateCountMap,
      Vendor vendor) {
    vendorTaskInformationDTO.setId(vendor.getId());
    vendorTaskInformationDTO.setName(vendor.getName());
    vendorTaskInformationDTO.setVendorCode(vendor.getVendorCode());
    vendorTaskInformationDTO.setCapacity(vendor.getQuota());
    vendorTaskInformationDTO.setAssignedCount(0);
    vendorTaskInformationDTO.setQcCount(0);
    for (Object state : stateCountMap.keySet()) {
      switch (state.toString()) {
        case CONTENT_AND_IMAGE_APPROVED: {
          vendorTaskInformationDTO.setQcCount(stateCountMap.get(state));
          break;
        }
        case IN_REVIEW: {
          vendorTaskInformationDTO
              .setAssignedCount(vendorTaskInformationDTO.getAssignedCount() + stateCountMap.get(state));
          break;
        }
        case CONTENT_APPROVED: {
          vendorTaskInformationDTO
              .setAssignedCount(vendorTaskInformationDTO.getAssignedCount() + stateCountMap.get(state));
          break;
        }
        case IMAGES_APPROVED: {
          vendorTaskInformationDTO
              .setAssignedCount(vendorTaskInformationDTO.getAssignedCount() + stateCountMap.get(state));
          break;
        }
        case IMAGE_REJECTED: {
          vendorTaskInformationDTO
              .setAssignedCount(vendorTaskInformationDTO.getAssignedCount() + stateCountMap.get(state));
          break;
        }
        case CONTENT_REJECTED: {
          vendorTaskInformationDTO
              .setAssignedCount(vendorTaskInformationDTO.getAssignedCount() + stateCountMap.get(state));
          break;
        }
        default:
          break;
      }
    }
  }

  @Override
  @Transactional
  public void deleteVender(Vendor vendor) throws Exception {
    if (vendor.getId() != null) {
      vendorRepository.setVendorQuota(vendor.getId());
      vendor.setMarkForDelete(true);
      vendorRepository.save(vendor);
    }
  }

  @Override public Integer assignedProductCount(String vendorCode) throws Exception {
    return vendorRepository.assignedProductCount(vendorCode);
  }

  @Override
  @Transactional(readOnly = false)
  public VendorDefaultFilter saveDefaultSettingFilter(VendorDefaultFilterRequest vendorDefaultFilterRequest,
      String storeId) throws JsonProcessingException {
    VendorDefaultFilter vendorDefaultFilter =
        vendorFilterRepository.findByStoreIdAndVendorEmailAndMarkForDeleteFalse(storeId,
            vendorDefaultFilterRequest.getVendorEmail());
    if (Objects.isNull(vendorDefaultFilter)) {
      vendorDefaultFilter = new VendorDefaultFilter();
      vendorDefaultFilter.setVendorEmail(vendorDefaultFilterRequest.getVendorEmail());
    }
    vendorDefaultFilter.setRequestedSkuCount(vendorDefaultFilterRequest.getRequestedSkuCount());
    vendorDefaultFilter.setAssigneeList(objectMapper.writeValueAsString(vendorDefaultFilterRequest.getAssigneeList()));
    vendorDefaultFilter.setStoreId(storeId);
    return vendorFilterRepository.saveAndFlush(vendorDefaultFilter);
  }

  @Override
  public VendorDefaultFilterResponse getDefaultSettingFilter(String storeId, String vendorEmail) throws IOException {
    VendorDefaultFilterResponse vendorDefaultFilterResponse = new VendorDefaultFilterResponse();
    VendorDefaultFilter vendorDefaultFilter = vendorFilterRepository.findByStoreIdAndVendorEmailAndMarkForDeleteFalse(storeId, vendorEmail);
    vendorDefaultFilterResponse.setVendorEmail(vendorDefaultFilter.getVendorEmail());
    vendorDefaultFilterResponse.setRequestedSkuCount(vendorDefaultFilter.getRequestedSkuCount());
    vendorDefaultFilterResponse.setAssigneeList(objectMapper.readValue(vendorDefaultFilter.getAssigneeList(), new TypeReference<List<String>>() {}));
    return vendorDefaultFilterResponse;
  }
}
