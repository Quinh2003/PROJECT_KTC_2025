#!/usr/bin/env python3
"""
KTC Logistics Workflow Diagram Generator - Simplified Version
-----------------------------------------------------------
Generates a Mermaid phases diagram for the KTC Logistics workflow.
Outputs directly to the diagrams directory as a .mmd file.

Usage:
  python draw_workflow.py                # Generate phases diagram .mmd file
  python draw_workflow.py --render       # Generate and export phases diagram to PNG
  python draw_workflow.py --format svg   # Generate and export phases diagram to SVG
"""

import os
import sys
import argparse
import subprocess
from pathlib import Path
from enum import Enum

class OutputFormat(Enum):
    PNG = "png"
    SVG = "svg"
    PDF = "pdf"
    MMD = "mmd"
    
# Core workflow data structure
WORKFLOW_PHASES = [
    {
        "phase": "Fleet Management",
        "color": "rgb(235,245,255)",
        "activities": [
            {"from": "Fleet Manager", "to": "Driver", "action": "Assign Vehicle to Driver"},
            {"from": "Fleet Manager", "to": "System", "action": "Register Vehicle-Driver Assignment"},
            {"from": "System", "to": "Fleet Manager", "action": "Confirm Assignment", "type": "response"},
            {"from": "Fleet Manager", "to": "Fleet Manager", "action": "Schedule Maintenance"},
        ]
    },
    {
        "phase": "Order Placement",
        "color": "rgb(255,240,240)",
        "activities": [
            {"from": "Customer", "to": "System", "action": "Register / Auth"},
            {"from": "Customer", "to": "System", "action": "Create Order"},
            {"from": "System", "to": "Dispatcher", "action": "Forward Order", "type": "response"},
            {"from": "Dispatcher", "to": "Customer", "action": "Quote & Confirmation"},
        ]
    },
    {
        "phase": "Dispatch",
        "color": "rgb(240,245,255)",
        "activities": [
            {"from": "Dispatcher", "to": "Dispatcher", "action": "Validate & Price"},
            {"from": "Dispatcher", "to": "Driver", "action": "Assign Driver"},
            {"from": "Dispatcher", "to": "System", "action": "Set Status(ASSIGNED)"},
        ]
    },
    {
        "phase": "Delivery Execution",
        "color": "rgb(235,255,235)",
        "activities": [
            {"from": "Driver", "to": "System", "action": "Accept Order"},
            {"from": "Driver", "to": "System", "action": "Start Delivery Journey"},
            {"from": "System", "to": "Dispatcher", "action": "Notify Delivery Started", "type": "response"},
            {"from": "Driver", "to": "System", "action": "Send Location Update (hourly)"},
            {"from": "System", "to": "Dispatcher", "action": "Forward Location Updates", "type": "response"},
            {"from": "Driver", "to": "System", "action": "Update Order Status"},
            {"from": "System", "to": "Customer", "action": "ETA & Status Notifications", "type": "response"},
            {"from": "Driver", "to": "Customer", "action": "Deliver & Proof (POD)"},
            {"from": "Customer", "to": "System", "action": "Confirm Receipt"},
            {"from": "System", "to": "Dispatcher", "action": "Update Order Status(DELIVERED)", "type": "response"},
            {"from": "Customer", "to": "System", "action": "Feedback / Rating"},
            {"from": "System", "to": "Dispatcher", "action": "Customer Satisfaction Update", "type": "response"},
        ]
    },
    {
        "phase": "Analytics & Operational Oversight",
        "color": "rgb(245,245,245)",
        "activities": [
            {"from": "System", "to": "System", "action": "Aggregate Events"},
            {"from": "Operations Manager", "to": "System", "action": "Performance Query"},
            {"from": "System", "to": "Operations Manager", "action": "Metrics & Dashboards", "type": "response"},
            {"from": "Operations Manager", "to": "Fleet Manager", "action": "Resource Rebalance"},
            {"from": "Operations Manager", "to": "Dispatcher", "action": "Optimization Directives"},
        ]
    },
    {
        "phase": "System Management",
        "color": "rgb(250,235,255)",
        "activities": [
            {"from": "Administrator", "to": "System", "action": "User / Role Updates"},
            {"from": "Administrator", "to": "System", "action": "Security Audit"},
            {"from": "System", "to": "Dispatcher", "action": "Feature Flags", "type": "response"},
            {"from": "System", "to": "Fleet Manager", "action": "Config Broadcast", "type": "response"},
        ]
    }
]

# Actor IDs for diagram
ACTOR_IDS = {
    "Customer": "CU",
    "Dispatcher": "DI",
    "Driver": "DR",
    "Fleet Manager": "FM",
    "Operations Manager": "OP",
    "Administrator": "AD",
    "System": "SY"
}

# Optional emoji icons for actors
ACTOR_ICONS = {
    "Customer": "👤",
    "Dispatcher": "📋",
    "Driver": "🧑‍✈️",
    "Fleet Manager": "🚚",
    "Operations Manager": "📊",
    "Administrator": "⚙️",
    "System": "🖥️"
}

def generate_phases_diagram() -> str:
    """Generate a compact phases diagram in Mermaid syntax"""
    
    mmd = ["%% KTC Logistics Workflow - Phased Sequence Diagram", "sequenceDiagram", "    autonumber"]
    
    # Add participants with emoji labels
    for actor, short_id in ACTOR_IDS.items():
        icon = ACTOR_ICONS.get(actor, "")
        mmd.append(f'    participant {short_id} as "{icon} {actor}"')
    
    mmd.append("")
    
    # Add phases and their activities
    for phase in WORKFLOW_PHASES:
        first_actor = phase["activities"][0]["from"]
        last_actor = phase["activities"][-1]["from"] if phase["activities"][-1]["to"] == phase["activities"][-1]["from"] else phase["activities"][-1]["to"]
        
        mmd.append(f'    rect {phase["color"]}')
        mmd.append(f'        note over {ACTOR_IDS[first_actor]},{ACTOR_IDS[last_actor]}: Phase {WORKFLOW_PHASES.index(phase)+1} – {phase["phase"]}')
        
        for activity in phase["activities"]:
            arrow = "-->" if activity.get("type") == "response" else "->>"
            mmd.append(f'        {ACTOR_IDS[activity["from"]]}{arrow}{ACTOR_IDS[activity["to"]]}: {activity["action"]}')
            
        mmd.append("    end")
        mmd.append("")
    
    return "\n".join(mmd)

def create_mermaid_file(content: str, output_path: str) -> None:
    """Create a .mmd file with the generated content"""
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(content)
    print(f"Created Mermaid file: {output_path}")

def export_diagram(mmd_path: str, output_format: OutputFormat) -> None:
    """Export the Mermaid diagram to various formats using mmdc CLI"""
    output_path = mmd_path.replace('.mmd', f'.{output_format.value}')
    
    try:
        # Use the mmdc CLI to convert Mermaid to output format
        print(f"Exporting diagram to {output_format.value.upper()}: {output_path}")
        
        try:
            result = subprocess.run(
                ["mmdc", "-i", mmd_path, "-o", output_path, "--backgroundColor", "white"],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                print(f"Successfully exported diagram to: {output_path}")
            else:
                print(f"Error while exporting: {result.stderr}")
                print("Using the Mermaid Live Editor is recommended: https://mermaid.live")
        except FileNotFoundError:
            print("mermaid-cli (mmdc) not found. You need to install it:")
            print("npm install -g @mermaid-js/mermaid-cli")
            
    except Exception as e:
        print(f"Error exporting diagram: {e}")
        print("Using the Mermaid Live Editor is recommended: https://mermaid.live")

def main():
    """Main entry point for the workflow diagram generator"""
    
    parser = argparse.ArgumentParser(description="Generate KTC Logistics workflow phases diagram")
    parser.add_argument("--render", action="store_true", help="Export the diagram using mermaid-cli")
    parser.add_argument("--format", choices=["png", "svg", "pdf"], default="png",
                        help="Output format when exporting (png, svg, pdf)")
    parser.add_argument("--output", "-o", default="diagrams",
                        help="Output directory for the generated files (default: diagrams)")
    
    args = parser.parse_args()
    
    # Convert arguments to enum
    output_format = OutputFormat(args.format)
    
    # Create output directory if it doesn't exist
    output_dir = Path(args.output)
    output_dir.mkdir(exist_ok=True)
    
    # Generate the phases diagram
    phases_content = generate_phases_diagram()
    phases_path = output_dir / "ktc_phases_diagram.mmd"
    create_mermaid_file(phases_content, str(phases_path))
    
    if args.render:
        export_diagram(str(phases_path), output_format)

if __name__ == "__main__":
    main()
