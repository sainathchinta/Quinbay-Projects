package com.gda.mta.product.dto;

import java.util.Date;
import java.util.List;

import com.gdn.common.web.base.BaseResponse;

public class ProductStockAlertResponse extends BaseResponse {

	private static final long serialVersionUID = 7184846437413603350L;
	private List<String> itemSku;

	public ProductStockAlertResponse() {
	}

	public ProductStockAlertResponse(String id, String storeId,
			Date createdDate, String createdBy, Date updatedDate,
			String updatedBy, List<String> itemSku) {
		super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
		this.itemSku = itemSku;
	}
	
	public List<String> getItemSku() {
		return itemSku;
	}

	public void setItemSku(List<String> itemSku) {
		this.itemSku = itemSku;
	}

	@Override
	public String toString() {
		return String
				.format("ProductCollectionResponse [itemSku=%s]",
						itemSku, getItemSku());
	}

}
