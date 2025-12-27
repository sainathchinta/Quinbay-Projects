package com.gdn.x.mta.distributiontask.service.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.VendorDefaultFilter;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;

import java.io.IOException;
import java.util.List;

import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * Created by Alok on 9/16/16.
 */
public interface VendorService {

 /**
  * To Save The Vendor
  *
  * @param vendor
  * @return
   */
 Vendor save(Vendor vendor);

 /**
  * To Fetch the Vendor List
  *
  * @param pageable
  * @return
   */
 Page<Vendor> findVendorList(Pageable pageable);

 /**
  * To Fetch Vendor By Vendor Code
  *
  * @param vendorCode
  * @return
   */
 Vendor findByVendorCode(String vendorCode);
 
 /**
  * Get vendors capacity
  * 
  * @return
  */
 List<VendorCapacityDTO> countVendorsCapacity();
 
 /**
  * Fetch vendor assignation and capacity information
  * 
  * @return
  */
 List<VendorTaskInformationDTO> countVendorAssignationAndCapacity() throws IOException, SolrServerException;

 /**
  * deleting the vendor
  * @param vendor
  * @throws Exception
  */
 void deleteVender(Vendor vendor) throws Exception;

 /**
  * Calculating the  products count still vendor is working on it before deletion of vendor
  *
  * @param vendorCode
  * @return
  * @throws Exception
  */
 Integer assignedProductCount(String vendorCode) throws Exception;

 /**
  * Saving vendor default filter settings
  *
  * @param storeId
  * @param vendorDefaultFilterRequest
  * @return
  */
 VendorDefaultFilter saveDefaultSettingFilter(VendorDefaultFilterRequest vendorDefaultFilterRequest, String storeId)
     throws JsonProcessingException;

 /**
  * Get vendor default setting using storeId and vendorEmail
  *
  * @param storeId
  * @param vendorEmail
  * @return
  * @throws IOException
  */
 VendorDefaultFilterResponse getDefaultSettingFilter(String storeId, String vendorEmail) throws IOException;
}
