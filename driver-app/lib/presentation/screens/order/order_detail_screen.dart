import 'dart:ui';
import 'package:flutter/material.dart';
import 'package:timeline_tile/timeline_tile.dart';

class OrderDetailScreen extends StatefulWidget {
  final String orderId;
  
  const OrderDetailScreen({
    Key? key,
    required this.orderId,
  }) : super(key: key);

  @override
  State<OrderDetailScreen> createState() => _OrderDetailScreenState();
}

class _OrderDetailScreenState extends State<OrderDetailScreen> {
  final _pageController = PageController();
  int _currentPage = 0;
  bool _isLoading = false;
  bool _isUpdating = false;
  
  final List<String> _statusOptions = [
    'Đã tiếp nhận',
    'Đang vận chuyển',
    'Đang giao hàng',
    'Đã giao hàng',
    'Hoàn thành',
  ];
  
  @override
  void dispose() {
    _pageController.dispose();
    super.dispose();
  }
  
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: GlassButton(
          onTap: () => Navigator.pop(context),
          child: Icon(
            Icons.arrow_back,
            color: Colors.blueGrey.shade800,
          ),
        ),
        title: Text(
          'Chi tiết đơn hàng #${widget.orderId}',
          style: TextStyle(
            color: Colors.blueGrey.shade800,
            fontWeight: FontWeight.bold,
          ),
        ),
        actions: [
          GlassButton(
            onTap: () {},
            child: Icon(
              Icons.more_vert,
              color: Colors.blueGrey.shade800,
            ),
          ),
        ],
      ),
      body: Container(
        decoration: BoxDecoration(
          gradient: LinearGradient(
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
            colors: [
              Color(0xFFEDF1FD),
              Color(0xFFF0F4FF),
              Color(0xFFE8EEFF),
            ],
          ),
        ),
        child: SafeArea(
          child: _isLoading
              ? Center(child: CircularProgressIndicator())
              : Column(
                  children: [
                    _buildPageIndicator(),
                    Expanded(
                      child: PageView(
                        controller: _pageController,
                        onPageChanged: (index) {
                          setState(() {
                            _currentPage = index;
                          });
                        },
                        children: [
                          _buildDetailsPage(),
                          _buildRoutePage(),
                          _buildTimelinePage(),
                        ],
                      ),
                    ),
                  ],
                ),
        ),
      ),
      bottomNavigationBar: _buildBottomBar(),
    );
  }
  
  Widget _buildPageIndicator() {
    final titles = ['Chi tiết', 'Lộ trình', 'Trạng thái'];
    
    return Container(
      height: 60,
      margin: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.6),
        borderRadius: BorderRadius.circular(20),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.05),
            blurRadius: 10,
            offset: Offset(0, 2),
          ),
        ],
      ),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceEvenly,
        children: titles.asMap().entries.map((entry) {
          final index = entry.key;
          final title = entry.value;
          final isSelected = _currentPage == index;
          
          return GestureDetector(
            onTap: () {
              _pageController.animateToPage(
                index,
                duration: Duration(milliseconds: 300),
                curve: Curves.easeInOut,
              );
            },
            child: AnimatedContainer(
              duration: Duration(milliseconds: 200),
              padding: EdgeInsets.symmetric(
                horizontal: 16,
                vertical: 8,
              ),
              decoration: BoxDecoration(
                color: isSelected
                    ? Colors.blue.withOpacity(0.1)
                    : Colors.transparent,
                borderRadius: BorderRadius.circular(12),
                border: isSelected
                    ? Border.all(color: Colors.blue)
                    : null,
              ),
              child: Text(
                title,
                style: TextStyle(
                  color: isSelected
                      ? Colors.blue
                      : Colors.blueGrey.shade700,
                  fontWeight: isSelected
                      ? FontWeight.bold
                      : FontWeight.normal,
                ),
              ),
            ),
          );
        }).toList(),
      ),
    );
  }
  
  Widget _buildDetailsPage() {
    return SingleChildScrollView(
      padding: EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          GlassContainer(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          'Mã đơn #${widget.orderId}',
                          style: TextStyle(
                            fontSize: 18,
                            fontWeight: FontWeight.bold,
                            color: Colors.blue.shade800,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          'Tạo lúc: 08:30 25/10/2023',
                          style: TextStyle(
                            fontSize: 12,
                            color: Colors.blueGrey.shade600,
                          ),
                        ),
                      ],
                    ),
                    Container(
                      padding: EdgeInsets.symmetric(
                        horizontal: 12,
                        vertical: 6,
                      ),
                      decoration: BoxDecoration(
                        color: Colors.blue.shade100,
                        borderRadius: BorderRadius.circular(12),
                      ),
                      child: Text(
                        'Đang giao hàng',
                        style: TextStyle(
                          color: Colors.blue.shade800,
                          fontWeight: FontWeight.bold,
                          fontSize: 12,
                        ),
                      ),
                    ),
                  ],
                ),
                SizedBox(height: 16),
                Divider(color: Colors.grey.withOpacity(0.2)),
                SizedBox(height: 16),
                _buildInfoRow(
                  title: 'Khách hàng',
                  value: 'Công ty TNHH ABC',
                  icon: Icons.business,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Loại hàng',
                  value: 'Thiết bị điện tử',
                  icon: Icons.category,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Trọng lượng',
                  value: '243 kg',
                  icon: Icons.scale,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Phương thức thanh toán',
                  value: 'Chuyển khoản',
                  icon: Icons.payment,
                ),
              ],
            ),
          ),
          SizedBox(height: 16),
          Text(
            'Địa chỉ',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 8),
          _buildAddressCard(
            title: 'Điểm lấy hàng',
            address: 'Kho KTC Logistics - 123 Đường ABC, Quận 1, TP. HCM',
            time: '09:00 25/10/2023',
            isPickup: true,
          ),
          SizedBox(height: 12),
          _buildAddressCard(
            title: 'Điểm giao hàng',
            address: 'Tòa nhà XYZ - 456 Đường DEF, Quận 7, TP. HCM',
            time: 'Dự kiến: 15:30 25/10/2023',
            isPickup: false,
          ),
          SizedBox(height: 16),
          Text(
            'Thông tin tài xế',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 8),
          GlassContainer(
            child: Row(
              children: [
                CircleAvatar(
                  radius: 30,
                  backgroundImage: NetworkImage(
                    'https://randomuser.me/api/portraits/men/32.jpg',
                  ),
                ),
                SizedBox(width: 16),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        'Nguyễn Văn A',
                        style: TextStyle(
                          fontSize: 16,
                          fontWeight: FontWeight.bold,
                          color: Colors.blueGrey.shade800,
                        ),
                      ),
                      SizedBox(height: 4),
                      Text(
                        'Tài xế KTC Logistics',
                        style: TextStyle(
                          fontSize: 14,
                          color: Colors.blueGrey.shade600,
                        ),
                      ),
                      SizedBox(height: 4),
                      Row(
                        children: [
                          Icon(
                            Icons.star,
                            size: 16,
                            color: Colors.amber,
                          ),
                          SizedBox(width: 4),
                          Text(
                            '4.8',
                            style: TextStyle(
                              fontSize: 14,
                              fontWeight: FontWeight.bold,
                              color: Colors.blueGrey.shade800,
                            ),
                          ),
                          Text(
                            ' (142 đánh giá)',
                            style: TextStyle(
                              fontSize: 12,
                              color: Colors.blueGrey.shade600,
                            ),
                          ),
                        ],
                      ),
                    ],
                  ),
                ),
                IconButton(
                  onPressed: () {},
                  icon: Icon(
                    Icons.phone,
                    color: Colors.green,
                  ),
                ),
              ],
            ),
          ),
          SizedBox(height: 16),
          Text(
            'Thông tin xe',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 8),
          GlassContainer(
            child: Column(
              children: [
                _buildInfoRow(
                  title: 'Biển số xe',
                  value: '51F-123.45',
                  icon: Icons.directions_car,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Loại xe',
                  value: 'Xe tải 2.5 tấn',
                  icon: Icons.local_shipping,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Màu sắc',
                  value: 'Trắng',
                  icon: Icons.palette,
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildRoutePage() {
    return SingleChildScrollView(
      padding: EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          GlassContainer(
            height: 300,
            child: Center(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(
                    Icons.map,
                    size: 48,
                    color: Colors.blue.shade400,
                  ),
                  SizedBox(height: 16),
                  Text(
                    'Bản đồ lộ trình',
                    style: TextStyle(
                      fontSize: 18,
                      fontWeight: FontWeight.bold,
                      color: Colors.blueGrey.shade800,
                    ),
                  ),
                  SizedBox(height: 8),
                  Text(
                    'Hiển thị bản đồ Google Maps ở đây',
                    style: TextStyle(
                      fontSize: 14,
                      color: Colors.blueGrey.shade600,
                    ),
                  ),
                ],
              ),
            ),
          ),
          SizedBox(height: 16),
          Text(
            'Thông tin lộ trình',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 8),
          GlassContainer(
            child: Column(
              children: [
                _buildInfoRow(
                  title: 'Tổng quãng đường',
                  value: '15.7 km',
                  icon: Icons.route,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Thời gian dự kiến',
                  value: '45 phút',
                  icon: Icons.access_time,
                ),
                SizedBox(height: 12),
                _buildInfoRow(
                  title: 'Mức tiêu thụ nhiên liệu',
                  value: '2.1 lít',
                  icon: Icons.local_gas_station,
                ),
              ],
            ),
          ),
          SizedBox(height: 16),
          Text(
            'Trạng thái hiện tại',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 8),
          GlassContainer(
            child: Column(
              children: [
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          'Đang trên đường giao hàng',
                          style: TextStyle(
                            fontSize: 16,
                            fontWeight: FontWeight.bold,
                            color: Colors.blue.shade800,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          'Cập nhật: 14:32 25/10/2023',
                          style: TextStyle(
                            fontSize: 12,
                            color: Colors.blueGrey.shade600,
                          ),
                        ),
                      ],
                    ),
                    Container(
                      padding: EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color: Colors.blue.shade100,
                        shape: BoxShape.circle,
                      ),
                      child: Icon(
                        Icons.local_shipping,
                        color: Colors.blue.shade800,
                      ),
                    ),
                  ],
                ),
                SizedBox(height: 16),
                LinearProgressIndicator(
                  value: 0.65,
                  backgroundColor: Colors.grey.shade200,
                  valueColor: AlwaysStoppedAnimation<Color>(
                    Colors.blue.shade400,
                  ),
                  minHeight: 8,
                  borderRadius: BorderRadius.circular(4),
                ),
                SizedBox(height: 8),
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Text(
                      'Đã đi được 10.2 km',
                      style: TextStyle(
                        fontSize: 12,
                        color: Colors.blueGrey.shade600,
                      ),
                    ),
                    Text(
                      'Còn lại 5.5 km',
                      style: TextStyle(
                        fontSize: 12,
                        color: Colors.blueGrey.shade600,
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildTimelinePage() {
    final statusEvents = [
      {
        'status': 'Đã tiếp nhận',
        'time': '08:30 25/10/2023',
        'desc': 'Đơn hàng đã được tiếp nhận bởi KTC Logistics',
        'isDone': true,
      },
      {
        'status': 'Đang vận chuyển',
        'time': '09:15 25/10/2023',
        'desc': 'Đơn hàng đã được tài xế Nguyễn Văn A tiếp nhận và đang vận chuyển',
        'isDone': true,
      },
      {
        'status': 'Đang giao hàng',
        'time': '14:30 25/10/2023',
        'desc': 'Đơn hàng đang được giao đến người nhận',
        'isDone': true,
      },
      {
        'status': 'Đã giao hàng',
        'time': 'Chưa thực hiện',
        'desc': 'Đơn hàng đã được giao cho người nhận',
        'isDone': false,
      },
      {
        'status': 'Hoàn thành',
        'time': 'Chưa thực hiện',
        'desc': 'Đơn hàng đã hoàn thành và xác nhận thanh toán',
        'isDone': false,
      },
    ];
    
    return SingleChildScrollView(
      padding: EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          GlassContainer(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  'Cập nhật trạng thái đơn hàng',
                  style: TextStyle(
                    fontSize: 16,
                    fontWeight: FontWeight.bold,
                    color: Colors.blueGrey.shade800,
                  ),
                ),
                SizedBox(height: 16),
                DropdownButtonFormField<String>(
                  value: _statusOptions[2], // Đang giao hàng
                  decoration: InputDecoration(
                    filled: true,
                    fillColor: Colors.white.withOpacity(0.5),
                    border: OutlineInputBorder(
                      borderRadius: BorderRadius.circular(12),
                      borderSide: BorderSide(
                        color: Colors.grey.shade300,
                      ),
                    ),
                    contentPadding: EdgeInsets.symmetric(
                      horizontal: 16,
                      vertical: 16,
                    ),
                  ),
                  items: _statusOptions.map((status) {
                    return DropdownMenuItem<String>(
                      value: status,
                      child: Text(status),
                    );
                  }).toList(),
                  onChanged: (value) {},
                ),
                SizedBox(height: 16),
                SizedBox(
                  width: double.infinity,
                  child: ElevatedButton(
                    onPressed: _isUpdating ? null : () {},
                    style: ElevatedButton.styleFrom(
                      backgroundColor: Colors.blue.shade400,
                      foregroundColor: Colors.white,
                      padding: EdgeInsets.symmetric(vertical: 16),
                      shape: RoundedRectangleBorder(
                        borderRadius: BorderRadius.circular(12),
                      ),
                      elevation: 0,
                    ),
                    child: _isUpdating
                        ? SizedBox(
                            width: 24,
                            height: 24,
                            child: CircularProgressIndicator(
                              color: Colors.white,
                              strokeWidth: 2,
                            ),
                          )
                        : Text('Cập nhật trạng thái'),
                  ),
                ),
              ],
            ),
          ),
          SizedBox(height: 24),
          Text(
            'Lịch sử trạng thái',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.blueGrey.shade800,
            ),
          ),
          SizedBox(height: 16),
          Container(
            decoration: BoxDecoration(
              color: Colors.white.withOpacity(0.5),
              borderRadius: BorderRadius.circular(20),
              boxShadow: [
                BoxShadow(
                  color: Colors.black.withOpacity(0.05),
                  blurRadius: 10,
                  offset: Offset(0, 3),
                ),
              ],
            ),
            child: ClipRRect(
              borderRadius: BorderRadius.circular(20),
              child: BackdropFilter(
                filter: ImageFilter.blur(sigmaX: 5, sigmaY: 5),
                child: Container(
                  padding: EdgeInsets.symmetric(vertical: 20),
                  child: Column(
                    children: statusEvents.asMap().entries.map((entry) {
                      final index = entry.key;
                      final event = entry.value;
                      final isLast = index == statusEvents.length - 1;
                      
                      return TimelineTile(
                        alignment: TimelineAlign.manual,
                        lineXY: 0.2,
                        isFirst: index == 0,
                        isLast: isLast,
                        indicatorStyle: IndicatorStyle(
                          width: 24,
                          height: 24,
                          indicator: Container(
                            decoration: BoxDecoration(
                              color: event['isDone'] as bool
                                  ? Colors.blue.shade400
                                  : Colors.grey.shade300,
                              shape: BoxShape.circle,
                              border: Border.all(
                                color: Colors.white,
                                width: 2,
                              ),
                            ),
                            child: event['isDone'] as bool
                                ? Icon(
                                    Icons.check,
                                    size: 16,
                                    color: Colors.white,
                                  )
                                : null,
                          ),
                        ),
                        beforeLineStyle: LineStyle(
                          color: Colors.blue.shade200,
                          thickness: 2,
                        ),
                        afterLineStyle: LineStyle(
                          color: index < 2
                              ? Colors.blue.shade200
                              : Colors.grey.shade300,
                          thickness: 2,
                        ),
                        endChild: Container(
                          padding: EdgeInsets.only(
                            left: 16,
                            right: 16,
                            top: 16,
                            bottom: isLast ? 16 : 32,
                          ),
                          child: Column(
                            crossAxisAlignment: CrossAxisAlignment.start,
                            children: [
                              Text(
                                event['status'] as String,
                                style: TextStyle(
                                  fontSize: 16,
                                  fontWeight: FontWeight.bold,
                                  color: event['isDone'] as bool
                                      ? Colors.blueGrey.shade800
                                      : Colors.grey.shade500,
                                ),
                              ),
                              SizedBox(height: 4),
                              Text(
                                event['desc'] as String,
                                style: TextStyle(
                                  fontSize: 14,
                                  color: event['isDone'] as bool
                                      ? Colors.blueGrey.shade600
                                      : Colors.grey.shade500,
                                ),
                              ),
                            ],
                          ),
                        ),
                        startChild: Container(
                          padding: EdgeInsets.only(
                            right: 16,
                            top: 16,
                            bottom: isLast ? 16 : 32,
                          ),
                          child: Text(
                            event['time'] as String,
                            textAlign: TextAlign.right,
                            style: TextStyle(
                              fontSize: 12,
                              color: event['isDone'] as bool
                                  ? Colors.blueGrey.shade600
                                  : Colors.grey.shade500,
                            ),
                          ),
                        ),
                      );
                    }).toList(),
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildInfoRow({
    required String title,
    required String value,
    required IconData icon,
  }) {
    return Row(
      children: [
        Container(
          padding: EdgeInsets.all(8),
          decoration: BoxDecoration(
            color: Colors.blue.shade50,
            borderRadius: BorderRadius.circular(8),
          ),
          child: Icon(
            icon,
            size: 18,
            color: Colors.blue.shade700,
          ),
        ),
        SizedBox(width: 12),
        Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              title,
              style: TextStyle(
                fontSize: 12,
                color: Colors.blueGrey.shade600,
              ),
            ),
            SizedBox(height: 2),
            Text(
              value,
              style: TextStyle(
                fontSize: 14,
                fontWeight: FontWeight.w500,
                color: Colors.blueGrey.shade800,
              ),
            ),
          ],
        ),
      ],
    );
  }
  
  Widget _buildAddressCard({
    required String title,
    required String address,
    required String time,
    required bool isPickup,
  }) {
    return GlassContainer(
      child: Row(
        children: [
          Container(
            padding: EdgeInsets.all(12),
            decoration: BoxDecoration(
              color: isPickup
                  ? Colors.green.shade50
                  : Colors.orange.shade50,
              borderRadius: BorderRadius.circular(12),
            ),
            child: Icon(
              isPickup ? Icons.flight_takeoff : Icons.flight_land,
              color: isPickup
                  ? Colors.green.shade700
                  : Colors.orange.shade700,
            ),
          ),
          SizedBox(width: 16),
          Expanded(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  title,
                  style: TextStyle(
                    fontSize: 16,
                    fontWeight: FontWeight.bold,
                    color: Colors.blueGrey.shade800,
                  ),
                ),
                SizedBox(height: 4),
                Text(
                  address,
                  style: TextStyle(
                    fontSize: 14,
                    color: Colors.blueGrey.shade600,
                  ),
                ),
                SizedBox(height: 4),
                Text(
                  time,
                  style: TextStyle(
                    fontSize: 12,
                    color: Colors.grey.shade600,
                    fontStyle: FontStyle.italic,
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildBottomBar() {
    return Container(
      padding: EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.7),
        borderRadius: BorderRadius.vertical(
          top: Radius.circular(24),
        ),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.05),
            blurRadius: 10,
            offset: Offset(0, -3),
          ),
        ],
      ),
      child: ClipRRect(
        borderRadius: BorderRadius.vertical(
          top: Radius.circular(24),
        ),
        child: BackdropFilter(
          filter: ImageFilter.blur(sigmaX: 10, sigmaY: 10),
          child: Row(
            children: [
              Expanded(
                child: ElevatedButton.icon(
                  onPressed: () {},
                  icon: Icon(Icons.navigation),
                  label: Text('Điều hướng'),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.blue.shade400,
                    foregroundColor: Colors.white,
                    padding: EdgeInsets.symmetric(vertical: 16),
                    shape: RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(12),
                    ),
                    elevation: 0,
                  ),
                ),
              ),
              SizedBox(width: 12),
              Expanded(
                child: ElevatedButton.icon(
                  onPressed: () {},
                  icon: Icon(Icons.call),
                  label: Text('Gọi điện'),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.white,
                    foregroundColor: Colors.blue.shade700,
                    padding: EdgeInsets.symmetric(vertical: 16),
                    shape: RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(12),
                      side: BorderSide(color: Colors.blue.shade200),
                    ),
                    elevation: 0,
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}

class GlassButton extends StatelessWidget {
  final VoidCallback onTap;
  final Widget child;
  
  const GlassButton({
    Key? key,
    required this.onTap,
    required this.child,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: onTap,
      child: Container(
        margin: EdgeInsets.all(8),
        padding: EdgeInsets.all(8),
        decoration: BoxDecoration(
          color: Colors.white.withOpacity(0.3),
          borderRadius: BorderRadius.circular(12),
          border: Border.all(
            color: Colors.white.withOpacity(0.5),
            width: 1.5,
          ),
        ),
        child: child,
      ),
    );
  }
}

class GlassContainer extends StatelessWidget {
  final Widget child;
  final double? height;
  
  const GlassContainer({
    Key? key,
    required this.child,
    this.height,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      height: height,
      padding: EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.5),
        borderRadius: BorderRadius.circular(20),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.05),
            blurRadius: 10,
            offset: Offset(0, 3),
          ),
        ],
      ),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(20),
        child: BackdropFilter(
          filter: ImageFilter.blur(sigmaX: 5, sigmaY: 5),
          child: child,
        ),
      ),
    );
  }
}


