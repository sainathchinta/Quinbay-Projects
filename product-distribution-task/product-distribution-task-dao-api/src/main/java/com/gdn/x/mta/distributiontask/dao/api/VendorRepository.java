package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.Vendor;

/**
 * Created by virajjasani on 15/09/16.
 */
public interface VendorRepository extends JpaRepository<Vendor, String> {

  String QUERY_COUNT_ALL_VENDOR_ASSIGNATION_AND_INQC_AND_CAPACITY =
      "select id,vendor_code as vendorCode,name,sum(assigned) as assignedCount,sum(in_qc) as qcCount,quota as capacity from "
      + "("
      + "select pv.id,pv.vendor_code,pv.name,pv.quota, count(ppdt.id) as assigned, 0 in_qc "
      + "from pdt_vendor pv left join pdt_product_distribution_task ppdt on ppdt.vendor=pv.id "
      + "and ppdt.state in ('IN_REVIEW','CONTENT_APPROVED','IMAGES_APPROVED','CONTENT_REJECTED','IMAGE_REJECTED') and ppdt.mark_for_delete=false "
      + "where pv.mark_for_delete=false "
      + "group by pv.id,pv.vendor_code,pv.name,pv.quota "
      + "union "
      + "select pv.id,pv.vendor_code,pv.name,pv.quota, 0 as assigned, count(ppdt.id) as in_qc "
      + "from pdt_vendor pv left join pdt_product_distribution_task ppdt on ppdt.vendor=pv.id "
      + "and ppdt.state in ('CONTENT_AND_IMAGE_APPROVED') and ppdt.mark_for_delete=false "
      + "where pv.mark_for_delete=false "
      + "group by pv.id,pv.vendor_code,pv.name,pv.quota"
      + ") as count_assignee "
      + "group by id,vendor_code,name,quota order by name";

  @Query(value = "SELECT ID FROM PDT_VENDOR WHERE VENDOR_CODE = ?1 AND MARK_FOR_DELETE IS FALSE",
      nativeQuery = true)
  String getVendorIdByVendorCode(String vendorCode) throws Exception;

  @Query("select vendor From Vendor vendor where markForDelete = false Order By vendor.createdDate")
  Page<Vendor> getVendorList(Pageable pageable);

  Vendor findByVendorCodeAndMarkForDeleteFalse(String vendorCode);

  List<Vendor> findByMarkForDeleteFalse();

  @Query(
      value = "select pv.id,pv.vendor_code,pv.name,pv.start_holiday_date,pv.end_holiday_date,pv.quota-COALESCE(pvqc.total_review_in_progress,0) as "
          + "remaining_capacity "
          + "from pdt_vendor pv left join pdt_vendor_quota_counter pvqc on pv.id=pvqc.vendor where pv.mark_for_delete=false",
      nativeQuery = true)
  List<Object[]> countAllVendorRemainingCapacity();

  @Query(value = QUERY_COUNT_ALL_VENDOR_ASSIGNATION_AND_INQC_AND_CAPACITY, nativeQuery = true)
  List<Object[]> countAllVendorAssignationAndInQCAndCapacity();

  @Query(value = "select  count(ppdt.id) as assigned from pdt_vendor pv left join pdt_product_distribution_task ppdt on ppdt.vendor=pv.id and ppdt.state in ('CONTENT_APPROVED','IMAGES_APPROVED','CONTENT_AND_IMAGE_APPROVED','PASSED') and ppdt.mark_for_delete=false where pv.vendor_code =?1 and pv.mark_for_delete=false ", nativeQuery = true)
  Integer assignedProductCount(String vendorCode) throws Exception;

  @Modifying
  @Query(value = "update pdt_vendor_quota_counter vqc set mark_for_delete = true where vqc.vendor = :id", nativeQuery = true)
  void setVendorQuota(@Param("id") String id);

}
