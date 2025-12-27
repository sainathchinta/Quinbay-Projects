package com.gdn.partners.bulk.util;

public interface CnExcelHeaderInfo {
  String PRODUCT_NAME_EN = "Enter the product name \n (max. 150 characters).";
  String DESCRIPTION_EN = "Enter the product \n description \n (max. \n 5000 characters).";
  String MODEL_EAN_UPC_EN = "Enter the Unique \n Product Code (number \n on the barcode) \n (should \n be 5,8,12 or 13 \n digits without any \n alphanumeric \n characters).";
  String SELLER_SKU_EN = "Enter SKU \n (max. 50 characters).";
  String UNIQUE_SELLING_POINT_EN = "Enter the unique selling points \n of your product \n (max. 400 characters).";
  String BUYABLE_EN = "Set this flag to \n ON if you want \n to start selling \n the product";
  String COLOUR_FAMILY_EN = "Enter the \n family color";
  String COLOUR_EN = "Enter the color \n of the product \n (if applicable) ";
  String PARENT_EN = "Enter the \n same code \n here to link all \n variants";
  String IMAGES_EN = "You can enter the website address (link) for product images.\n"
      + "To generate the URL, first upload the image to https://imgur.com/upload \n" + "To get the Image URL: \n"
      + "1. In Google Chrome, right click on the image \n" + "2. Click\"'Open Link in a New Tab\"\n"
      + "3. Copy the link and then enter it in this column\n" + "OR\n"
      + "You can also enter the names of the image of your product (First image is mandatory, Image format: JPG, JPEG and PNG, image name should not have special characters).";
  String URL_VIDEO_EN =
      "You can add product \n videos to explain, use \n and demonstrate the \n product (We support \n only Youtube links as \n of now).";
  String HANDLING_TYPE_EN = "Enter the shipping \n type ( Regular; \n Shipment by Seller \n Partner; Bopis)";
  String PICKUP_POINT_CODE_EN = "Enter the \n store/warehouse \n location from the \n \"Toko\" sheet";
  String LENGTH_EN = "Enter the length of \n the product in cms";
  String WIDTH_EN = "Enter the breadth of \n the product in cms";
  String HEIGHT_EN = "Enter the height of \n the product in cms";
  String WEIGHT_EN = "Enter the weight of \n the product in grams";
  String PRICE_EN = "Enter the \n normal price \n (before discount)";
  String SELLING_PRICE_EN = "Enter the \n selling price";
  String AVAILABLE_STOCK_EN =
      "Please fill with \n your available stock. \n" + "Fill with 0 if \n you don't have \n stock for now.";
  String MINIMUM_STOCK_EN =
      "Please fill with \n minimum stock. \n You will get \n notifications \n when \n the stock has \n reached the \n minimum stock.";
  String DELIVERY_STATUS_EN = "Online = 1, Offline = 0";
  String CNC_STATUS_EN = "On = 1, Off = 0";
  String RECOMMENDED_ATTRIBUTE_EN =
      "Please fill the recommended attributes to increase the \n score of your product and boost visibility on search results";
  String BRAND_EN =
      "Please enter the brand of the product \n from the brands listed in \"All Brands \n List\". tab. Please select 'No brand' \n or 'OEM' if you can not find the brand \n name in the brand list.";
  String OTHER_ATTRIBUTE_EN =
      "Please fill these attributes to provide more information about \n the product to the customers";

  // for Indonesian seller
  String NAMA_PRODUCK_IN = "Masukkan nama produk (maks. 150 \n karakter).";
  String DESKRIPSI_IN = "Buat deskripsi \n produk yang \n menarik supaya \n banyak \n pelanggan melirik. \n (maks. 5000 \n karakter).";
  String MODEL_EAN_UPC_IN = "Masukkan Unique \n Product Code (nomor \n pada barcode). \n (hanus \n be 5,8,12 atau 13 \n digit tanpa \n karakter \n alfanumerik).";
  String SELLER_SKU_IN = "Masukkan \n detail SKU \n seller (maks.\n 50 karakter).";
  String KEUNGGULAN_PRODUK_IN =
      "Ceritakan keunggulan produk \n Anda yang membedakan \n dengan produk lainnya \n (maks.400 karakter).";
  String BISA_DIBELI_IN = "Aktifkan flag \n ini jika Anda \n ingin menjual \n produk di Blibli.";
  String COLOUR_FAMILY_IN = "Masukkan family \n color (jika ada).";
  String WARNA_IN = "Masukkan warna \n produk (jika ada).";
  String PARENT_IN = "Masukkan kode \n yang sama di \n sini untuk \n menghubungkan \n semua varian.\n";
  String FOTO_IN = "Anda bisa memasukkan alamat web (link) untuk foto product.\n"
      + "Untuk membuat link URL, upoload foto ke https://imgur.com/upload terlebih dulu.\n"
      + "Untuk mendapatkan URL foto: \n" + "1. Pada Google Chrome,klik kanan pada foto \n"
      + "2. Klik 'Open Link in a New Tab' \n" + "3. Salin link dan masukkan ke kolom ini\n" + "ATAU\n"
      + "Masukkan foto produk Anda (wajib upload min.1 foto, format foto: JPG, JPEG dan PNG, nama foto tidak boleh memiliki karakter khusus).";
  String URL_VIDEO_IN =
      "Anda bisa \n menambahkan video \n produk untuk menjelaskan, \n menggunakan, \n dan demo produk\n (Hanya bisa link \n Youtube)";
  String TIPE_PENANGAN_IN = "Masukkan jenis \n pengiriman (Reguler; \n pengiriman oleh \n seller, BOPIS).";
  String KODE_PICKUP_POINT_IN = "Masukkan lokasi toko\n  / gudang dari sheet \n \"Toko\".";
  String PANJANG_IN = "Panjang produk \n dalam satuan cm.";
  String LEBAR_IN = "Lebar produk \n dalam satuan cm.";
  String TINGGI_IN = "Tinggi produk \n dalam satuan cm.";
  String BERAT_IN = "Berat produk dalam \n satuan gram.";
  String HARGA_IN = "Tentukan \n harga normal \n (sebelum diskon).";
  String HARGA_PENJULAN_IN = "Tentukan \n harga jual.";
  String STOK_TERSEDIA_IN = "Silakan isi stok \n produk. Isi \n dengan 0 jika \n tidak tersedia.";
  String STOK_MINIMUM_IN =
      "Silakan isi \n dengan jumlah \n stok minimum \n yang diinginkan. \n Anda akan mendapatkan \n notifikasi jika stok \n telah mencapai batas \n minimum yang ditentukan.";
  String RECOMMENDED_ATTRIBUTE_IN =
      "Opsional untuk diisi untuk meningkatkan skor produk agar \n produk Anda makin mudah dicari.";
  String BRAND_IN = "Masukkan brand produk Anda dari \"All \n Brands List\". \n"
      + "Silakan pilih \"No Brand\" atau \"OEM\" \n jika Anda tidak bisa menemukan nama \n brand di daftar brand.";
  String OTHER_ATTRIBUTE_IN = "Silakan isi dengan informasi tambahan bagi pelanggan Anda.";
  String SECONDARY_HEADER_BFB_BASE_PRICE_EN = "Please fill in BfB base price";
  String SECONDARY_HEADER_BFB_MANAGED_INFO_EN = "Yes=1, No=0";
  String SECONDARY_HEADER_BFB_STATUS_EN= "Online=1, Offline=0";
  String SECONDARY_HEADER_BFB_BASE_PRICE_IN = "Silakan isi BfB Harga dasar";
  String SECONDARY_HEADER_CHILD_SKU = "Use comma to separate child SKU (Ex: ABC 12345, DEF 56789)";
  String SECONDARY_HEADER_QUANTITY = "Use comma to separate quantity (Ex: 1, 2)";
  String SECONDARY_HEADER_CHILD_SKU_ID = "Gunakan koma untuk memisahkan SKU child (Cth: ABC 12345, DEF 56789)";
  String SECONDARY_HEADER_QUANTITY_ID = "Gunakan koma untuk memisahkan jumlah (Cth: 1, 2)";
}
